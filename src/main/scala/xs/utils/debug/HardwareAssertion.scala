package xs.utils.debug

import chisel3._
import chisel3.experimental.hierarchy.core.IsLookupable
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.{FileRegisters, ResetRRArbiter}
import chisel3.experimental.{SourceInfo, SourceLine}
import org.chipsalliance.cde.config.{Field, Parameters}

case object HardwareAssertionKey extends Field[HwaParams]

case class HwaParams(
  enable: Boolean = false,
  maxInfoBits: Int = 16,
  maxAssertRepeatNum: Int = 4,
  hwaDevDepth: Int = 2048
)

class HwAsrtBundle(width: Int) extends Bundle {
  val id = Output(UInt(width.W))
}

case class HwAsrtNode(
  assertion: DecoupledIO[HwAsrtBundle],
  desc: Seq[(Int, String)],
  level: Int
)

case class DomainInfo(desc: Seq[(Int, String)]) extends IsLookupable

object HardwareAssertion {
  private var assertId = 0
  private var hwaNodesSeq = Seq[HwAsrtNode]()

  /** Checks for a condition to be valid in the circuit at rising clock edge
   * when not in reset. If the condition evaluates to false, the circuit
   * simulation stops with an error. The assert id and user bits will be
   * output to the module interface
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param desc optional format string to print when the assertion fires
   * @note desc must be defined as Printable(e.g. cf"xxx") to print chisel-type values
   */
  def apply(cond: Bool, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = {
    // EDA
    val cfSourceInfo = s match {
      case SourceLine(filename, line, col) => cf"$filename:$line:$col: "
      case _ => cf""
    }
    val assertCond = Mux(XsDebugGlobal.hwaCond, cond, true.B)
    assert(assertCond, cfSourceInfo + desc)
    // Hardware
    val hwaP = p(HardwareAssertionKey)
    if(hwaP.enable) {
      val assertCnt = RegInit(hwaP.maxAssertRepeatNum.U)
      val assertion = Wire(Decoupled(new HwAsrtBundle(log2Ceil(assertId + 2))))
      assertion.valid := !assertCond & assertCnt.orR
      assertion.bits.id := assertId.U
      assertCnt := Mux(assertion.fire, assertCnt - 1.U, assertCnt)
      require(assertion.bits.getWidth <= hwaP.maxInfoBits, s"$assertId hardware assertions are too many!")
      val sSourceInfo = s match {
        case SourceLine(filename, line, col) => s"$filename:$line:$col: "
        case _ => s""
      }
      val node = HwAsrtNode(assertion, Seq((assertId, sSourceInfo + desc)), 0)
      hwaNodesSeq = hwaNodesSeq :+ node
      assertId = assertId + 1
    }
  }
  def apply(cond: Bool)(implicit p: Parameters, s: SourceInfo): Unit = apply(cond, cf"")

  /** Apply an assertion in the hardware design with an enable signal.
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param en   enable signal for the assertion
   * @param desc optional format string to print when the assertion fires
   * @note desc must be defined as Printable(e.g. cf"xxx") to print chisel-type values
   */
  def withEn(cond: Bool, en: Bool, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = apply(Mux(en, cond, true.B), desc)
  def withEn(cond: Bool, en: Bool)(implicit p: Parameters, s: SourceInfo): Unit = withEn(cond, en, cf"")

  /** Checks for timeout condition by counting cycles since last clear signal.
   * If the counter reaches its maximum value (300_0000 cycles), the circuit
   * simulation stops with an error. The assert id and user bits will be
   * output to the module interface.
   *
   * @param clear   reset signal that clears the timeout counter when asserted
   * @param timeout EDA assert max timeout value
   * @param desc    optional format string to print when timeout occurs
   * @note desc must be defined as Printable (e.g. cf"xxx") to print chisel-type values
   * @note Default timeout threshold of 3,000,000 cycles corresponds to 1ms at 3GHz clock frequency
   */
  def checkTimeout(clear: Bool, timeout: Int, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = {
    // At 3Ghz, 1ms equals 300_0000 cycles.
    val cnt = Counter(0 until 3000000, reset = clear)
    apply(!cnt._2, desc)(p, s)
    // EDA
    val cfSourceInfo = s match {
      case SourceLine(filename, line, col) => cf"$filename:$line:$col: "
      case _ => cf""
    }
    assert(cnt._1 < timeout.U, cfSourceInfo + desc)
  }

  def checkTimeout(clear: Bool, timeout: Int)(implicit p: Parameters, s: SourceInfo): Unit = {
    checkTimeout(clear, timeout, cf"")(p, s)
  }

  def placePipe(level: Int, moduleTop: Boolean = false)(implicit p: Parameters): HwAsrtNode = {
    if(p(HardwareAssertionKey).enable && hwaNodesSeq.count(_.level < level) != 0) {
      val children = hwaNodesSeq.filter(_.level < level)
      val maxId = children.flatMap(_.desc).map(_._1).max
      val idBits = log2Ceil(maxId + 2)
      val hwaQ = Module(new Queue(new HwAsrtBundle(idBits), entries = 2))
      val hwaArb = Module(new ResetRRArbiter(new HwAsrtBundle(idBits), children.size))
      val assertion = hwaQ.io.deq
      require(assertion.bits.getWidth <= p(HardwareAssertionKey).maxInfoBits, s"$maxId hardware assertions are too many!")
      require(hwaArb.io.in.size <= 32, s"HardwareAssertion arbiter input size[${hwaArb.io.in.size}] cannot exceed 32")
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
    } else {
      val assertion = WireInit(0.U.asTypeOf(Decoupled(new HwAsrtBundle(1))))
      HwAsrtNode(assertion, Seq((0, "")), 0)
    }
  }

  def fromDomain(aio: DecoupledIO[HwAsrtBundle], dinfo: DomainInfo, level: Int, domainName: String)(implicit p: Parameters): HwAsrtNode = {
    if(p(HardwareAssertionKey).enable) {
      val maxId = dinfo.desc.map(_._1).max + assertId
      val hwaQ = Module(new Queue(new HwAsrtBundle(log2Ceil(maxId + 2)), entries = 2))
      hwaQ.io.enq.valid := aio.valid
      hwaQ.io.enq.bits.id := aio.bits.id + assertId.U
      aio.ready := hwaQ.io.enq.ready
      require(hwaQ.io.enq.bits.getWidth <= p(HardwareAssertionKey).maxInfoBits, s"$maxId hardware assertions are too many!")
      val newDesc = dinfo.desc.map(d => (d._1 + assertId, domainName + " " + d._2))
      val node = HwAsrtNode(assertion = hwaQ.io.deq, desc = newDesc, level = level)
      hwaNodesSeq = hwaNodesSeq :+ node
      assertId = newDesc.map(_._1).max + 1
      node
    } else {
      val assertion = WireInit(0.U.asTypeOf(Decoupled(new HwAsrtBundle(1))))
      HwAsrtNode(assertion, Seq((0, "")), 0)
    }
  }

  def release(n: HwAsrtNode, dir: String, pfx: String = "")(implicit p: Parameters): Unit = {
    if(p(HardwareAssertionKey).enable) {
      hwaNodesSeq = Seq()
      assertId = 0
      val str = n.desc
        .map(d => s"assertion ${d._1}: ${d._2}")
        .reduce((a, b) => a + '\n' + b)
      FileRegisters.add(dir, s"${pfx}_hardware_assertion.txt", str, dontCarePrefix = true)
    }
  }
}

object XsDebugGlobal {
  var hwaCond = true.B
}

final class XsDebugWhenContext(
  cond: Option[() => Bool],
  block: => Any,
  altConds: List[() => Bool]) {

  XsDebugGlobal.hwaCond = {
    val alt = altConds.foldRight(true.B) {
      case (c, acc) => acc & !c()
    }
    cond.map(alt && _()).getOrElse(alt)
  }

  def elsewhen(elseCond: => Bool)(block: => Any): XsDebugWhenContext = {
    new XsDebugWhenContext(Some(() => elseCond), block, cond ++: altConds)
  }

  def otherwise(block: => Any): Unit =
    new XsDebugWhenContext(None, block, cond ++: altConds)

  block
  XsDebugGlobal.hwaCond = true.B
}

object awhen {
  def apply(cond: Bool)(block: => Any): XsDebugWhenContext = {
    new XsDebugWhenContext(Some(() => cond), block, Nil)
  }
}