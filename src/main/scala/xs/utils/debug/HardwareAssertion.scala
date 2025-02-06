package xs.utils.debug

import chisel3._
import chisel3.experimental.hierarchy.core.IsLookupable
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.{FileRegisters, ResetRRArbiter}
import chisel3.experimental.{SourceInfo, SourceLine}

class HwAsrtBundle(width:Int) extends Bundle {
  val id    = Output(UInt(width.W))
  val user  = Output(UInt(48.W))
}

case class HwAsrtNode (
  assertion: DecoupledIO[HwAsrtBundle],
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
    val hwaQ = Module(new Queue(new HwAsrtBundle(log2Ceil(assertId + 2)), entries = 2))
    val assertion = hwaQ.io.deq
    val _user = Cat(user.reverse)
    require(_user.getWidth <= assertion.bits.user.getWidth, s"The bits width of the args([UInt[${_user.getWidth}.W]]) exceeds the upper limit(UInt[${assertion.bits.user.getWidth}.W])")
    hwaQ.io.enq.valid      := !cond
    hwaQ.io.enq.bits.id    := assertId.U
    hwaQ.io.enq.bits.user  := _user
    val sSourceInfo = s match {
      case SourceLine(filename, line, col) => s"$filename:$line:$col: "
      case _ => s""
    }
    val node = HwAsrtNode(assertion, Seq((assertId, sSourceInfo + desc)), 0)
    hwaNodesSeq = hwaNodesSeq :+ node
    assertId = assertId + 1
  }
  def apply(cond:Bool, user:UInt*)(implicit s: SourceInfo):Unit = apply(cond, cf"", Cat(user.reverse))

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
  def withEn(cond:Bool, en:Bool, user:UInt*)(implicit s: SourceInfo):Unit = withEn(cond, en, cf"", Cat(user.reverse))

  /** Checks for timeout condition by counting cycles since last clear signal.
   * If the counter reaches its maximum value (300_0000 cycles), the circuit
   * simulation stops with an error. The assert id and user bits will be
   * output to the module interface.
   *
   * @param clear   reset signal that clears the timeout counter when asserted
   * @param timeout EDA assert max timeout value
   * @param desc    optional format string to print when timeout occurs
   * @param user    optional bits to output some message to module interface
   * @note desc must be defined as Printable (e.g. cf"xxx") to print chisel-type values
   * @note user bits total width cant be over 48.W
   * @note Default timeout threshold of 3,000,000 cycles corresponds to 1ms at 3GHz clock frequency
   */
  def checkTimeout(clear:Bool, timeout: Int, desc:Printable, user:UInt*)(implicit s: SourceInfo):Unit = {
    // At 3Ghz, 1ms equals 300_0000 cycles.
    val cnt = Counter(0 until 3000000, reset = clear)
    apply(!cnt._2, desc, Cat(user.reverse))
    // EDA
    val cfSourceInfo = s match {
      case SourceLine(filename, line, col) => cf"$filename:$line:$col: "
      case _ => cf""
    }
    assert(cnt._1 < timeout.U, cfSourceInfo + desc)
  }
  def checkTimeout(clear:Bool, timeout: Int, user:UInt*)(implicit s: SourceInfo):Unit = checkTimeout(clear, timeout, cf"", Cat(user.reverse))


  def placePipe(level:Int, moduleTop:Boolean = false)(implicit s: SourceInfo):HwAsrtNode = {
    val children = hwaNodesSeq.filter(_.level < level)
    val maxId = children.flatMap(_.desc).map(_._1).max
    val idBits = log2Ceil(maxId + 2)
    val hwaQ = Module(new Queue(new HwAsrtBundle(idBits), entries = 2))
    val hwaArb = Module(new ResetRRArbiter(new HwAsrtBundle(idBits), children.size))
    val assertion = hwaQ.io.deq
    hwaNodesSeq = hwaNodesSeq.filterNot(_.level < level)
    require(children.nonEmpty)
    for(idx <- children.indices) {
      hwaArb.io.in(idx) <> BoringUtils.bore(children(idx).assertion)
    }
    val desc = children.flatMap(_.desc)
    hwaQ.io.enq <> hwaArb.io.out
    val node = HwAsrtNode(assertion, desc, level)
    if(!moduleTop) {
      hwaNodesSeq = hwaNodesSeq :+ node
    } else {
      assertId = 0
    }
    node
  }

  def fromDomain(aio:DecoupledIO[HwAsrtBundle], dinfo:DomainInfo, level:Int, domainName:String):HwAsrtNode = {
    val maxId = dinfo.desc.map(_._1).max + assertId
    val hwaQ = Module(new Queue(new HwAsrtBundle(log2Ceil(maxId + 2)), entries = 2))
    hwaQ.io.enq.valid := aio.valid
    hwaQ.io.enq.bits.id := aio.bits.id + assertId.U
    hwaQ.io.enq.bits.user := aio.bits.user
    aio.ready := hwaQ.io.enq.ready
    val newDesc = dinfo.desc.map(d => (d._1 + assertId, domainName + " " + d._2))
    val node = HwAsrtNode(assertion = hwaQ.io.deq, desc = newDesc, level = level)
    hwaNodesSeq = hwaNodesSeq :+ node
    assertId = newDesc.map(_._1).max + 1
    node
  }

  def setTopNode(n: HwAsrtNode):Unit = {
    topNode = Some(n)
    assertId = 0
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