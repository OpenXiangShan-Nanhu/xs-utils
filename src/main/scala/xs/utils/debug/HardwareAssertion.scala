package xs.utils.debug

import chisel3._
import chisel3.experimental.hierarchy.core.IsLookupable
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.FileRegisters
import chisel3.experimental.{SourceInfo, SourceLine}

class HwAsrtBundle(width:Int) extends Valid[UInt](UInt(width.W))

case class HwAsrtNode (
  assertion: HwAsrtBundle,
  desc: Seq[(Int, String)],
  level: Int
)

case class DomainInfo(desc: Seq[(Int, String)]) extends IsLookupable

object HardwareError {
  def apply(cond:Bool, desc:String):Unit = HardwareAssertion(!cond, desc)
  def apply(cond:Bool, en:Bool, desc:String):Unit = HardwareAssertion(!cond, en, desc)
}

object HardwareAssertion {
  private var assertId = 0
  private var hwaNodesSeq = Seq[HwAsrtNode]()
  private var topNode:Option[HwAsrtNode] = None

  def apply(cond:Bool, desc:String)(implicit s: SourceInfo):Unit = {
    // Hardware
    val assertion = Wire(new HwAsrtBundle(log2Ceil(assertId + 2)))
    assertion.valid := RegNext(!cond, false.B)
    assertion.bits := RegEnable(assertId.U, !cond)
    val node = HwAsrtNode(assertion, Seq((assertId, desc)), 0)
    hwaNodesSeq = hwaNodesSeq :+ node
    assertId = assertId + 1
    // EDA
    val debugInfo = s match {
      case SourceLine(filename, line, col) => s"[$filename:$line:$col]: "
      case _ => ""
    }
    assert(cond, debugInfo + desc)
  }

  def apply(cond:Bool, en:Bool, desc:String)(implicit s: SourceInfo):Unit = apply(Mux(en, cond, true.B), desc)

  def placePipe(level:Int, moduleTop:Boolean = false):HwAsrtNode = {
    val children = hwaNodesSeq.filter(_.level < level)
    val maxId = children.flatMap(_.desc).map(_._1).max
    val idBits = log2Ceil(maxId + 2)
    val assertion = Wire(new HwAsrtBundle(idBits))
    val hwaBds = Seq.fill(children.length)(Wire(new HwAsrtBundle(idBits)))
    hwaNodesSeq = hwaNodesSeq.filterNot(_.level < level)
    require(children.nonEmpty)
    for(idx <- children.indices) {
      hwaBds(idx) := BoringUtils.bore(children(idx).assertion)
    }
    val desc = children.flatMap(_.desc)
    val valid = Cat(hwaBds.map(_.valid)).orR
    val id = PriorityMux(hwaBds.map(_.valid), hwaBds.map(_.bits))
    val node = HwAsrtNode(assertion, desc, level)
    if(!moduleTop) {
      hwaNodesSeq = hwaNodesSeq :+ node
    } else {
      assertId = 0
    }
    assertion.valid := RegNext(valid, false.B)
    assertion.bits := RegEnable(id, valid)
    node
  }

  def fromDomain(aio:HwAsrtBundle, dinfo:DomainInfo, level:Int, domainName:String):HwAsrtNode = {
    val maxId = dinfo.desc.map(_._1).max + assertId
    val newAssertion = Wire(new HwAsrtBundle(log2Ceil(maxId + 2)))
    newAssertion.valid := RegNext(aio.valid, false.B)
    newAssertion.bits := RegEnable(aio.bits + assertId.U, aio.valid)
    val newDesc = dinfo.desc.map(d => (d._1 + assertId, domainName + " " + d._2))
    val node = HwAsrtNode(assertion = newAssertion, desc = newDesc, level = level)
    hwaNodesSeq = hwaNodesSeq :+ node
    assertId = newDesc.map(_._1).max + 1
    node
  }

  def setTopNode(n: HwAsrtNode):Unit = {
    topNode = Some(n)
  }

  def release(dir: String):Unit = {
    if(topNode.isDefined) {
      val str = topNode.get.desc
        .map(d => s"assertion ${d._1}: ${d._2}")
        .reduce((a, b) => a + '\n' + b)
      FileRegisters.add(dir, "hardware_assertion.txt", str, dontCarePrefix = true)
    } else {
      require(false)
    }
  }
}