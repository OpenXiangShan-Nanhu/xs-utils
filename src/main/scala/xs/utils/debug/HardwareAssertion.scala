package xs.utils.debug

import chisel3._
import chisel3.experimental.hierarchy.core.IsLookupable
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.FileRegisters
import chisel3.experimental.{SourceInfo, SourceLine}

class HwAsrtBundle(width:Int) extends Bundle {
  val valid = Output(Bool())
  val id    = Output(UInt(width.W))
  val user  = Output(UInt(48.W))
}

case class HwAsrtNode (
  assertion: HwAsrtBundle,
  desc: Seq[(Int, String)],
  level: Int
)

case class DomainInfo(desc: Seq[(Int, String)]) extends IsLookupable

object HardwareAssertion {
  private var assertId = 0
  private var hwaNodesSeq = Seq[HwAsrtNode]()
  private var topNode:Option[HwAsrtNode] = None

  /** Checks for a condition to be valid in the circuit at rising clock edge
   * when not in reset. If the condition evaluates to false, the circuit
   * simulation stops with an error. The assert id and user bits will be
   * output to the module interface
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param desc optional format string to print when the assertion fires
   * @param user optional bits to output some message to module interface
   * @note desc must be defined as Printable(e.g. cf"xxx") to print chisel-type values
   * @note user bits total width cant be over 48.W
   */
  def apply(cond:Bool, desc:Printable, user:UInt*)(implicit s: SourceInfo):Unit = {
    // EDA
    val cfSourceInfo = s match {
      case SourceLine(filename, line, col) => cf"$filename:$line:$col: "
      case _ => cf""
    }
    assert(cond, cfSourceInfo + desc)
    // Hardware
    val assertion = Wire(new HwAsrtBundle(log2Ceil(assertId + 2)))
    val _user = Cat(user.reverse)
    require(_user.getWidth <= assertion.user.getWidth, s"The bits width of the args([UInt[${_user.getWidth}.W]]) exceeds the upper limit(UInt[${assertion.user.getWidth}.W])")
    assertion.valid := RegNext(!cond, false.B)
    assertion.id    := RegEnable(assertId.U, !cond)
    assertion.user  := RegEnable(_user, !cond)
    val sSourceInfo = s match {
      case SourceLine(filename, line, col) => s"$filename:$line:$col: "
      case _ => s""
    }
    val node = HwAsrtNode(assertion, Seq((assertId, sSourceInfo + desc)), 0)
    hwaNodesSeq = hwaNodesSeq :+ node
    assertId = assertId + 1
  }

  /** Apply an assertion in the hardware design with an enable signal.
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param en   enable signal for the assertion
   * @param desc optional format string to print when the assertion fires
   * @param user optional bits to output some message to module interface
   * @note desc must be defined as Printable(e.g. cf"xxx") to print chisel-type values
   * @note user bits total width cant be over 48.W
   */
  def withEn(cond:Bool, en:Bool, desc:Printable, user:UInt*)(implicit s: SourceInfo):Unit = apply(Mux(en, cond, true.B), desc, Cat(user.reverse))

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
    val id = PriorityMux(hwaBds.map(_.valid), hwaBds.map(_.id))
    val user = PriorityMux(hwaBds.map(_.valid), hwaBds.map(_.user))
    val node = HwAsrtNode(assertion, desc, level)
    if(!moduleTop) {
      hwaNodesSeq = hwaNodesSeq :+ node
    } else {
      assertId = 0
    }
    assertion.valid := RegNext(valid, false.B)
    assertion.id    := RegEnable(id, valid)
    assertion.user  := RegEnable(user, valid)
    node
  }

  def fromDomain(aio:HwAsrtBundle, dinfo:DomainInfo, level:Int, domainName:String):HwAsrtNode = {
    val maxId = dinfo.desc.map(_._1).max + assertId
    val newAssertion = Wire(new HwAsrtBundle(log2Ceil(maxId + 2)))
    newAssertion.valid := RegNext(aio.valid, false.B)
    newAssertion.id := RegEnable(aio.id + assertId.U, aio.valid)
    newAssertion.user := RegEnable(aio.user, aio.valid)
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