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
import chisel3.util.experimental.BoringUtils

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
    val array_depth: Seq[Int])
      extends BaseNode {
    var children:              Seq[BaseNode] = Seq()
    var ramParamsBelongToThis: Seq[Ram2MbistParams] = Seq()
    require(level > 0)
  }

  sealed class SramNode(bd: Ram2Mbist, ids: Seq[Int], een: Boolean) extends RamBaseNode(bd, ids, een)

  sealed class PipelineNodeSram(bd: MbistBus, level: Int, array_id: Seq[Int], array_depth: Seq[Int])
      extends PipelineBaseNode(bd, level, array_id, array_depth)

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
      children.map {
        case ram: RamBaseNode      => ram.bd.params.set
        case pl:  PipelineBaseNode => pl.bd.params.set
      }.max,
      (children.map {
        case ram: RamBaseNode      => ram.bd.params.dataWidth
        case pl:  PipelineBaseNode => pl.bd.params.dataWidth
      }).max,
      (children.map {
        case ram: RamBaseNode      => ram.bd.params.maskWidth
        case pl:  PipelineBaseNode => pl.bd.params.maskWidth
      }).max,
      (children.map {
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

  def addController(level: Int): PipelineBaseNode = {
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
    val node = new PipelineNodeSram(bd, level, ids, depth)
    node.children = children.map {
      case ram: RamBaseNode =>
        val mbist = Wire(ram.bd.cloneType)
        mbist := DontCare
        dontTouch(mbist)
        val boreChildrenBd = BoringUtils.bore(ram.bd)
        boreChildrenBd.addr := mbist.addr
        boreChildrenBd.addr_rd := mbist.addr_rd
        boreChildrenBd.wdata := mbist.wdata
        boreChildrenBd.wmask := mbist.wmask
        boreChildrenBd.re := mbist.re
        boreChildrenBd.we := mbist.we
        boreChildrenBd.selectedOH := mbist.selectedOH
        boreChildrenBd.array := mbist.array
        boreChildrenBd.ack := mbist.ack
        mbist.rdata := boreChildrenBd.rdata
        new SramNode(mbist, ram.array_id, ram.een)
      case pl: PipelineBaseNode =>
        val mbist = Wire(pl.bd.cloneType)
        mbist := DontCare
        dontTouch(mbist)
        val boreChildrenBd = BoringUtils.bore(pl.bd)
        boreChildrenBd.array := mbist.array
        boreChildrenBd.all := mbist.all
        boreChildrenBd.req := mbist.req
        boreChildrenBd.writeen := mbist.writeen
        boreChildrenBd.be := mbist.be
        boreChildrenBd.addr := mbist.addr
        boreChildrenBd.indata := mbist.indata
        boreChildrenBd.readen := mbist.readen
        boreChildrenBd.addr_rd := mbist.addr_rd
        mbist.ack := boreChildrenBd.ack
        mbist.outdata := boreChildrenBd.outdata
        new PipelineNodeSram(mbist, pl.level, pl.array_id, pl.array_depth)
    }
    node.ramParamsBelongToThis = children.flatMap({
      case ram: RamBaseNode =>
        ram.bd.params.getAllNodesParams()
      case pl: PipelineBaseNode =>
        pl.ramParamsBelongToThis
    })
    globalNodes = remain :+ node

    node
  }
}
