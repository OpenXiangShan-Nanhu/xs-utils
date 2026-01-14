/** *************************************************************************************
  * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2022 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package xs.utils.mbist

import chisel3._
import chisel3.experimental.hierarchy.{IsLookupable, Lookupable}
import chisel3.util.experimental.BoringUtils
import xs.utils.sram.SramHelper

object Mbist {
  val maxMbistDataWidth = 256

  protected[mbist] var globalNodes = Seq[BaseNode]()

  sealed trait BaseNode {
    val bd:          MbistCommonBundle
    val level:       Int
    val array_id:    Seq[Int]
    val array_depth: Seq[Int]
  }

  sealed class RamBaseNode(
    val bd:  Ram2Mbist,
    val ids: Seq[Int],
    val een: Boolean // early wen/ren
  ) extends BaseNode {
    override val level: Int = 0
    override val array_id = ids
    override val array_depth = Seq.fill(ids.length)(0)
  }

  sealed class PipelineBaseNode(
    val bd:          MbistBus,
    val level:       Int,
    val array_id:    Seq[Int],
    val array_depth: Seq[Int],
    val offset:      Int,
    val name:        String
  ) extends BaseNode with IsLookupable {
    var children:              Seq[BaseNode] = Seq()
    var ramParamsBelongToThis: Seq[Ram2MbistParams] = Seq()
    require(level > 0)
  }

  sealed class SramNode(bd: Ram2Mbist, ids: Seq[Int], een: Boolean) extends RamBaseNode(bd, ids, een)

  sealed class PipelineNodeSram(bd: MbistBus, level: Int, array_id: Seq[Int], array_depth: Seq[Int], offset:Int, name:String)
      extends PipelineBaseNode(bd, level, array_id, array_depth, offset, name)

  def inferMbistBusParamsFromParams(children: Seq[MbistBusParams]): MbistBusParams =
    MbistBusParams(
      children.map(_.array).max,
      children.map(_.set).max,
      children.map(_.dataWidth).max,
      children.map(_.maskWidth).max,
      children.map(_.hasDualPort).reduce(_ || _)
    )

  def inferMbistBusParams(children: Seq[BaseNode]): MbistBusParams =
    MbistBusParams(
      children.map(_.array_id).reduce(_ ++ _).max,
      children.map({
        case ram: RamBaseNode      => ram.bd.params.set
        case pl:  PipelineBaseNode => pl.bd.params.set
      }).max,
      children.map({
        case ram: RamBaseNode      => ram.bd.params.dataWidth
        case pl:  PipelineBaseNode => pl.bd.params.dataWidth
      }).max,
      children.map({
        case ram: RamBaseNode      => ram.bd.params.maskWidth
        case pl:  PipelineBaseNode => pl.bd.params.maskWidth
      }).max,
      children.map({
        case ram: RamBaseNode      => !ram.bd.params.singlePort
        case pl:  PipelineBaseNode => pl.bd.params.hasDualPort
      }).reduce(_ || _)
    )

  def addRamNode(bd: Ram2Mbist, ids: Seq[Int], een: Boolean): RamBaseNode = {
    val node = new SramNode(bd, ids, een)
    globalNodes = globalNodes :+ node
    node
  }

  def isMaxLevel(level: Int) = level == Int.MaxValue

  def addInstance(bus: MbistBus, node:PipelineBaseNode, mod:RawModule, instName:String):Unit = {
    require(SramHelper.getDomainID >= node.array_id.min)
    val offset = SramHelper.getDomainID - node.array_id.min
    val ids = node.array_id.map(_ + offset)
    val newNode = new PipelineNodeSram(bus, node.level, ids, node.array_depth, offset, s"${node.name}_ext")
    newNode.ramParamsBelongToThis = for(i <- node.ramParamsBelongToThis.indices) yield {
      val src = node.ramParamsBelongToThis(i)
      val holderStr = src.holder()
      src.copy(
        holder = () => s"${mod.pathName}.$instName.$holderStr"
      )
    }
    val add = node.array_id.max + 1 - node.array_id.min
    SramHelper.increaseDomainID(add)
    globalNodes = globalNodes :+ newNode
  }

  def addController(level: Int, instance:Boolean, name:String): PipelineBaseNode = {
    require(globalNodes.nonEmpty, "No nodes were created before implementing mbist controller!")
    val candidateNodes = globalNodes.filter(inst => inst.isInstanceOf[SramNode] || inst.isInstanceOf[PipelineNodeSram])
    val children = candidateNodes.filter(_.level < level)
    val remain = globalNodes.filterNot(children.contains(_))
    require(children.nonEmpty, "Mbist controller level setting is wrong or no children nodes were found!")
    val params = inferMbistBusParams(children)
    val bd = Wire(new MbistBus(params))
    bd := DontCare
    dontTouch(bd)
    val ids = children.flatMap(_.array_id)
    val depth = children.flatMap(_.array_depth.map(_ + 1))
    val node = new PipelineNodeSram(bd, level, ids, depth, 0, name)
    node.children = children.map {
      case ram: RamBaseNode =>
        val mbist = Wire(ram.bd.cloneType)
        val _mbist = BoringUtils.bore(ram.bd)
        _mbist.addr := mbist.addr
        _mbist.addr_rd := mbist.addr_rd
        _mbist.wdata := mbist.wdata
        _mbist.wmask := mbist.wmask
        _mbist.re := mbist.re
        _mbist.we := mbist.we
        _mbist.selectedOH := mbist.selectedOH
        _mbist.array := mbist.array
        _mbist.ack := mbist.ack
        _mbist.broadcast := mbist.broadcast
        mbist.rdata := _mbist.rdata
        new SramNode(mbist, ram.array_id, ram.een)
      case pl: PipelineBaseNode =>
        val mbist = Wire(pl.bd.cloneType)
        val _mbist = BoringUtils.bore(pl.bd)
        _mbist.array := mbist.array
        _mbist.all := mbist.all
        _mbist.req := mbist.req
        _mbist.writeen := mbist.writeen
        _mbist.be := mbist.be
        _mbist.addr := mbist.addr
        _mbist.indata := mbist.indata
        _mbist.readen := mbist.readen
        _mbist.addr_rd := mbist.addr_rd
        _mbist.broadcast := mbist.broadcast
        mbist.ack := _mbist.ack
        mbist.outdata := _mbist.outdata
        new PipelineNodeSram(mbist, pl.level, pl.array_id, pl.array_depth, pl.offset, pl.name)
    }
    node.ramParamsBelongToThis = children.flatMap({
      case ram: RamBaseNode =>
        ram.bd.params.getAllNodesParams()
      case pl: PipelineBaseNode =>
        pl.ramParamsBelongToThis
    })
    if(instance) {
      val dec = node.array_id.max + 1 - node.array_id.min
      SramHelper.increaseDomainID(-dec)
      globalNodes = remain
    } else {
      globalNodes = remain :+ node
    }

    node
  }
}
