package xs.utils.debug

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class HwAsrtBundle extends Bundle {
  val valid = Output(Bool())
  val id = Output(UInt(16.W))
}

case class HwAsrtNode (
  assertion: HwAsrtBundle,
  desc: Seq[(Int, String)],
  level: Int
)

object HardwareAssertion {
  private var domainId = 0
  private var hwaNodesSeq = Seq[HwAsrtNode]()

  def apply(cond:Bool, desc:String):Unit = {
    val assertion = Wire(new HwAsrtBundle)
    assertion.valid := cond
    assertion.id := domainId.U
    val node = HwAsrtNode(assertion, Seq((domainId, desc)), 0)
    hwaNodesSeq = hwaNodesSeq :+ node
    domainId = domainId + 1
  }

  def placePipe(level:Int):HwAsrtNode = {
    val assertion = Wire(new HwAsrtBundle)
    val children = hwaNodesSeq.filter(_.level < level)
    val hwaBds = Seq.fill(children.length)(Wire(new HwAsrtBundle))
    hwaNodesSeq = hwaNodesSeq.filterNot(_.level < level)
    require(children.nonEmpty)
    for(idx <- children.indices) {
      val bd = BoringUtils.bore(children(idx).assertion)
      hwaBds(idx) := bd
    }
    val desc = children.flatMap(_.desc)
    val valid = Cat(hwaBds.map(_.valid)).orR
    val id = PriorityMux(hwaBds.map(_.valid), hwaBds.map(_.id))
    val node = HwAsrtNode(assertion, desc, level)
    if(level != Int.MaxValue) hwaNodesSeq = hwaNodesSeq :+ node
    assertion.valid := RegNext(valid, false.B)
    assertion.id := RegEnable(id, valid)
    node
  }

  def fromDomain(tio:HwAsrtBundle, node:HwAsrtNode, level:Int):HwAsrtNode = {
    val assertion = Wire(new HwAsrtBundle)
    assertion := tio
    node.copy(assertion = assertion, level = level)
  }
}

class HardwareAssertion {

}
