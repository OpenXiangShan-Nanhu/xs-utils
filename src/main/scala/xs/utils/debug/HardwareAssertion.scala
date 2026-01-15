package xs.utils.debug

import chisel3._
import chisel3.experimental.hierarchy.IsLookupable
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.{FileRegisters, ResetRRArbiter}
import chisel3.experimental.{CheckBoring, EscapedWire, SourceInfo, SourceLine, SpecialWireInit, noPrefix}
import org.chipsalliance.cde.config.{Field, Parameters}

import scala.collection.mutable

case object HardwareAssertionKey extends Field[HwaParams]

case class HwaParams(
  enable: Boolean = false,
  maxInfoBits: Int = 16,
  maxAssertRepeatNum: Int = 4,
  hwaDevDepth: Int = 2048
)

class HAssertBundle(val node: HAssertNode)(implicit p:Parameters) extends Bundle {
  val hwaP = p(HardwareAssertionKey)
  val cond = Option.when(node.point)(Output(Bool()))
  val bus = Option.when(!node.point)(Decoupled(UInt(hwaP.maxInfoBits.W)))
}

case class HAssertNode(
  desc: Seq[(Int, String)] = Seq(),
  level: Int = 0,
  point: Boolean = false,
  name: String = ""
) extends IsLookupable

class HAssertHub(level:Int)(implicit p:Parameters) extends Module {
  HardwareAssertion.placePipe(level)
}

object HAssert {
  def apply(cond:Bool, desc:Printable)(implicit p: Parameters, s: SourceInfo):Unit = HardwareAssertion(cond, desc)(p, s)
  def apply(cond:Bool)(implicit p: Parameters, s: SourceInfo):Unit = HardwareAssertion(cond)(p, s)
  def withEn(cond: Bool, en: Bool, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = HardwareAssertion.withEn(cond, en, desc)(p ,s)
  def withEn(cond: Bool, en: Bool)(implicit p: Parameters, s: SourceInfo): Unit = HardwareAssertion.withEn(cond, en)(p ,s)
  def checkTimeout(clear: Bool, timeout: Int, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = HardwareAssertion.checkTimeout(clear, timeout, desc)(p, s)
  def checkTimeout(clear: Bool, timeout: Int)(implicit p: Parameters, s: SourceInfo): Unit = HardwareAssertion.checkTimeout(clear, timeout)(p, s)
  def placePipe(level: Int, moduleTop: Boolean = false, name:String = "")(implicit p: Parameters): Option[Seq[HAssertBundle]] = HardwareAssertion.placePipe(level, moduleTop, name)
  def release(node: Option[Seq[HAssertBundle]], dir: String, pfx: String = "")(implicit p: Parameters): Unit = HardwareAssertion.release(node, dir, pfx)
  def fromIO(buses:Option[MixedVec[HAssertBundle]])(implicit p: Parameters):Unit = HardwareAssertion.fromIO(buses)
  def exportIO(implicit p: Parameters): Option[MixedVec[HAssertBundle]] = HardwareAssertion.exportIO

  def placePipeHub(level: Int)(implicit p: Parameters): Unit = noPrefix {
    if(p(HardwareAssertionKey).enable && HardwareAssertion.getHwaSeq.count(_.node.level < level) != 0) noPrefix {
      val hwa_hub = Module(new HAssertHub(level))
    }
  }
}

object HardwareAssertion {
  var gid = 0
  private var hwaSeq = Seq[HAssertBundle]()

  private val hashToCountMap = mutable.Map[String, Int]()

  private def extractStr(pt:Printable): String = {
    pt match {
      case Printables(pts) => pts.map(extractStr).reduce(_ + _)
      case PString(str) => str
      case _ => ""
    }
  }

  def getHwaSeq: Seq[HAssertBundle] = hwaSeq

  private def squashPoints(pts: Seq[HAssertBundle])(implicit p: Parameters): Seq[HAssertBundle] = {
    val hwaP = p(HardwareAssertionKey)
    pts.foreach(pp => require(pp.node.level == 0))
    pts.groupBy(_.node.desc.head._2).map({case(desc, ns) =>
      val asrtCnt = RegInit(hwaP.maxAssertRepeatNum.U(log2Ceil(hwaP.maxAssertRepeatNum + 1).W))
      val asrtVlds = ns.map(n => {
        val bore = BoringUtils.bore(n)
        if(n.node.point) {
          RegNext(bore.cond.get, false.B)
        } else {
          bore.bus.get.ready := true.B
          RegNext(bore.bus.get.valid, false.B)
        }
      })
      val newNode = HAssertNode(desc = Seq((gid, desc)), name = ns.head.node.name)
      val squashCond = Wire(new HAssertBundle(newNode))
      squashCond.bus.get.valid := Cat(asrtVlds).orR && asrtCnt.orR
      squashCond.bus.get.bits := gid.U
      when(squashCond.bus.get.fire) {
        asrtCnt := asrtCnt - 1.U
      }
      gid = gid + 1
      squashCond
    }).toSeq
  }

  private def genDescStr(desc:Printable, s: SourceInfo):Printable = {
    s match {
      case SourceLine(filename, line, col) =>
        val fn = filename.replaceAll("\\\\", "/")
        cf"$fn:$line:$col: " + desc
      case _ => desc
    }
  }

  /** Checks for a condition to be valid in the circuit at rising clock edge
   * when not in reset. If the condition evaluates to false, the circuit
   * simulation stops with an error. The assert id and user bits will be
   * output to the module interface
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param desc optional format string to print when the assertion fires
   * @note desc must be defined as Printable(e.g. cf"xxx") to print chisel-type values
   */
  def apply(cond:Bool, desc:Printable)(implicit p: Parameters, s: SourceInfo): Unit = {
    val descStr = genDescStr(desc, s)
    val assertCond = cond
    assert(assertCond, descStr)(s)
    val hwaP = p(HardwareAssertionKey)
    if(hwaP.enable) {
      val pdesc = extractStr(descStr)
      val pdescShort = pdesc.split("\n").head
      val hashCode = s"${pdesc.hashCode}"
      if(!hashToCountMap.contains(hashCode)) {
        hashToCountMap.addOne((hashCode, 0))
      }
      hashToCountMap(hashCode) = hashToCountMap(hashCode) + 1
      val pcode = s"${hashCode}_${hashToCountMap(hashCode) - 1}"
      val node = HAssertNode(desc = Seq((0, pdescShort)), level = 0, point = true, pdescShort)
      val thisCond = EscapedWire(new HAssertBundle(node))
      thisCond.cond.get := !assertCond
      thisCond.suggestName(s"hwa_$pcode")
      SpecialWireInit(s, thisCond.cond.get, 0, prepend = true)
      hwaSeq = hwaSeq :+ thisCond
    }
  }
  def apply(cond: Bool)(implicit p: Parameters, s: SourceInfo): Unit = apply(cond, "")(p, s)

  /** Apply an assertion in the hardware design with an enable signal.
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param en   enable signal for the assertion
   * @param desc optional format string to print when the assertion fires
   * @note desc must be defined as Printable(e.g. cf"xxx") to print chisel-type values
   */
  def withEn(cond: Bool, en: Bool, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = apply(Mux(en, cond, true.B), desc)(p ,s)
  def withEn(cond: Bool, en: Bool)(implicit p: Parameters, s: SourceInfo): Unit = withEn(cond, en, "")(p ,s)

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
  def checkTimeout(clear: Bool, timeout: Int, desc: Printable)(implicit p: Parameters, s: SourceInfo): Bool = {
    val to_val = 0x1L << log2Ceil(3_000_000)
    require(timeout <= to_val)
    val to_cnt = Reg(UInt(log2Ceil(to_val + 1).W))
    when(clear || to_cnt < to_val.U) {
      to_cnt := Mux(clear, 0.U, to_cnt + 1.U)
    }
    val eda_err = to_cnt >= timeout.U
    val hwa_err = to_cnt >= to_val.U
    val descStr = genDescStr(desc, s)
    assert(!eda_err, descStr)
    apply(!hwa_err, desc)(p, s)
    eda_err
  }

  def checkTimeout(clear: Bool, timeout: Int)(implicit p: Parameters, s: SourceInfo): Bool = {
    checkTimeout(clear, timeout, cf"timeout!")(p, s)
  }

  def placePipe(level: Int, moduleTop: Boolean = false, name:String = "")(implicit p: Parameters): Option[Seq[HAssertBundle]] = {
    if(p(HardwareAssertionKey).enable && hwaSeq.count(_.node.level < level) != 0) {
      val candidates = hwaSeq.filter(h => h.node.level < level && CheckBoring(h))
      val children = candidates.filterNot(_.node.level == 0) ++ squashPoints(candidates.filter(_.node.level == 0))
      val width = p(HardwareAssertionKey).maxInfoBits
      require(gid < (1L << width), s"hwa id $gid exceeds upper limit ${1L << width}")
      val nrPipe = if(moduleTop) 1 else (children.size + 15) / 16
      val segLen = (children.size + nrPipe - 1) / nrPipe
      val childrenSegSeq = children.grouped(segLen).toSeq
      require(nrPipe == childrenSegSeq.size)
      val res = for(cs <- childrenSegSeq) yield {
        val hwa_n = HAssertNode(desc = cs.flatMap(_.node.desc), level = level, name = name)
        val hwa_out = Wire(new HAssertBundle(hwa_n))
        if(cs.size > 1) {
          val hwa_arb = Module(new ResetRRArbiter(gen = UInt(width.W), n = cs.size))
          val hwa_q = Module(new Queue(gen = UInt(width.W), entries = 2))
          hwa_arb.io.in.zip(cs).foreach({ case (a, b) =>
            val hwa = BoringUtils.bore(b).bus.get
            a.valid := hwa.valid
            hwa.ready := a.ready
            a.bits := hwa.bits
          })
          hwa_q.io.enq <> hwa_arb.io.out
          hwa_out.bus.get <> hwa_q.io.deq
        } else {
          hwa_out <> BoringUtils.bore(cs.head)
        }
        hwa_out
      }
      if(!moduleTop) {
        hwaSeq = hwaSeq.filterNot(h => h.node.level < level && CheckBoring(h)) ++ res
      } else {
        gid = 0
        hashToCountMap.clear()
      }
      Some(res)
    } else {
      None
    }
  }

  def fromIO(buses:Option[MixedVec[HAssertBundle]])(implicit p: Parameters):Unit = {
    val hwaP = p(HardwareAssertionKey)
    val _impl = buses.isDefined && hwaP.enable
    if(_impl) {
      val _buses = buses.get
      val allIds = _buses.flatMap(_.node.desc.map(_._1))
      val offset = gid - allIds.min
      val add = allIds.max + 1 - allIds.min
      gid = gid + add
      for(i <- _buses.indices) yield {
        val _nd = _buses(i).node.desc.map(e => (e._1 + offset, e._2))
        val _nn = HAssertNode(desc = _nd, level = _buses(i).node.level, name = s"${_buses(i).node.name}_ext")
        val hwa = Wire(new HAssertBundle(_nn))
        hwa <> _buses(i)
        hwa.bus.foreach(_.bits := _buses(i).bus.get.bits + offset.U)
        hwaSeq = hwaSeq :+ hwa
      }
    }
  }

  def exportIO(implicit p: Parameters): Option[MixedVec[HAssertBundle]] = {
    val hwaP = p(HardwareAssertionKey)
    if(hwaP.enable) {
      val candidates = hwaSeq.filter(CheckBoring(_))
      val children = candidates.filterNot(_.node.point) ++ squashPoints(candidates.filter(_.node.point))
      if(children.nonEmpty) {
        val _io = IO(MixedVec(children.map(c => new HAssertBundle(c.node))))
        children.zip(_io).foreach({ case (a, b) =>
          b <> BoringUtils.bore(a)
        })
        val allIds = children.flatMap(_.node.desc.map(_._1))
        val dec = allIds.max + 1 - allIds.min
        gid = gid - dec
        hwaSeq = hwaSeq.filterNot(CheckBoring(_))
        Some(_io)
      } else {
        None
      }
    } else {
      None
    }
  }

  def release(as: Option[Seq[HAssertBundle]], dir: String, pfx: String = ""): Unit = {
    as.foreach(_.foreach(a => {
      hwaSeq = Nil
      gid = 0
      val str = a.node.desc
        .map(d => s"assertion ${d._1}: ${d._2}")
        .reduce((a, b) => a + '\n' + b)
      FileRegisters.add(dir, s"${pfx}_hardware_assertion.txt", str, dontCarePrefix = true)
    }))
  }
}