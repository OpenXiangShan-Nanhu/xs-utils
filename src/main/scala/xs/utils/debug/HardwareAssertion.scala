package xs.utils.debug

import chisel3._
import chisel3.experimental.hierarchy.IsLookupable
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.{FileRegisters, ResetRRArbiter}
import chisel3.experimental.{CheckBoring, EscapedWire, SourceInfo, SourceLine, SpecialWireInit, noPrefix}
import org.chipsalliance.cde.config.{Field, Parameters}
import xs.utils.queue.FastQueue

import java.nio.file.Paths
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

case class HAssertDesc(
  id: Int,
  hardDesc: String,
  file: String,
  line: Int,
  column: Int
) extends IsLookupable {
  def withId(newId: Int): HAssertDesc = copy(id = newId)
  def identity: (String, String, Int, Int) = (hardDesc, file, line, column)
  def location: String = s"$file:$line:$column"
}

case class HAssertNode(
  desc: Seq[HAssertDesc] = Seq(),
  level: Int = 0,
  point: Boolean = false,
  name: String = ""
) extends IsLookupable

class HAssertHub(level:Int)(implicit p:Parameters) extends Module {
  HardwareAssertion.placePipe(level)
}

object HAssert {
  def apply(cond: Bool, hardDesc: String)(implicit p: Parameters, s: SourceInfo): Unit =
    HardwareAssertion(cond, hardDesc)(p, s)
  def apply(cond: Bool, hardDesc: String, softDesc: Printable)(implicit p: Parameters, s: SourceInfo): Unit =
    HardwareAssertion(cond, hardDesc, softDesc)(p, s)
  def withEn(cond: Bool, en: Bool, hardDesc: String)(implicit p: Parameters, s: SourceInfo): Unit =
    HardwareAssertion.withEn(cond, en, hardDesc)(p, s)
  def withEn(cond: Bool, en: Bool, hardDesc: String, softDesc: Printable)(implicit p: Parameters, s: SourceInfo): Unit =
    HardwareAssertion.withEn(cond, en, hardDesc, softDesc)(p, s)
  def checkTimeout(clear: Bool, timeout: Int, hardDesc: String)(implicit p: Parameters, s: SourceInfo): Bool =
    HardwareAssertion.checkTimeout(clear, timeout, hardDesc)(p, s)
  def checkTimeout(clear: Bool, timeout: Int, hardDesc: String, softDesc: Printable)(
    implicit p: Parameters,
    s: SourceInfo
  ): Bool = HardwareAssertion.checkTimeout(clear, timeout, hardDesc, softDesc)(p, s)
  def placePipe(level: Int, moduleTop: Boolean = false, name:String = "")(
    implicit p: Parameters
  ): Option[Seq[HAssertBundle]] = HardwareAssertion.placePipe(level, moduleTop, name)
  def release(node: Option[Seq[HAssertBundle]], dir: String, pfx: String, runtimeSourceTypes: Seq[String])(
    implicit p: Parameters
  ): Unit = HardwareAssertion.release(node, dir, pfx, runtimeSourceTypes)
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
  private type ImportSignature = Vector[(Int, String, Vector[HAssertDesc])]
  private val importedLayoutOffsets = new java.util.IdentityHashMap[AnyRef, mutable.Map[ImportSignature, Int]]()

  private case class ManifestRef(
    producer: String,
    runtimeSourceTypes: Seq[String],
    file: String,
    assertionCount: Int,
    idWidth: Int
  )
  private val releasedManifests = mutable.Map[String, ManifestRef]()
  private var renderedCatalog: Option[String] = None

  def getHwaSeq: Seq[HAssertBundle] = hwaSeq

  private def normalizeSourcePath(filename: String): String = {
    val source = Paths.get(filename).toAbsolutePath.normalize()
    val root = sys.env
      .get("ZHUJIANG_SOURCE_ROOT")
      .map(Paths.get(_).toAbsolutePath.normalize())
      .getOrElse(Paths.get("").toAbsolutePath.normalize())
    require(source.startsWith(root), s"HAssert source $source is outside source root $root")
    root.relativize(source).toString.replace('\\', '/')
  }

  private def sourceDesc(hardDesc: String, s: SourceInfo): HAssertDesc = {
    require(hardDesc.trim.nonEmpty, "HAssert hardDesc must be non-empty")
    s match {
      case SourceLine(filename, line, col) =>
        HAssertDesc(0, hardDesc.trim, normalizeSourcePath(filename), line, col)
      case _ => throw new IllegalArgumentException(s"HAssert $hardDesc requires source line information")
    }
  }

  private def simulationDesc(desc: HAssertDesc, softDesc: Option[Printable]): Printable = {
    val hard = PString(s"Hardware error: ${desc.hardDesc}\nLocation: ${desc.location}")
    softDesc match {
      case Some(soft) => hard + PString("\nDebug context: ") + soft
      case None => hard
    }
  }

  private def dedupDesc(desc: Seq[HAssertDesc]): Seq[HAssertDesc] = {
    desc.groupBy(_.id).toSeq.sortBy(_._1).map { case (id, entries) =>
      val metadata = entries.map(e => (e.hardDesc, e.file, e.line, e.column)).distinct
      require(metadata.size == 1, s"hwa id $id maps to multiple descriptions: ${metadata.mkString(", ")}")
      entries.head
    }
  }

  private def importSignature(buses: Seq[HAssertBundle]): ImportSignature = {
    buses.map(b => (b.node.level, b.node.name, dedupDesc(b.node.desc).toVector)).toVector
  }

  private def importOffset(buses: Seq[HAssertBundle]): Int = {
    val scope = Module.currentModule.getOrElse(
      throw new IllegalStateException("HAssert.fromIO must be called inside a module")
    )
    val scopeOffsets = Option(importedLayoutOffsets.get(scope)).getOrElse {
      val offsets = mutable.Map[ImportSignature, Int]()
      importedLayoutOffsets.put(scope, offsets)
      offsets
    }
    val signature = importSignature(buses)
    scopeOffsets.getOrElseUpdate(signature, {
      val allIds = buses.flatMap(_.node.desc.map(_.id))
      val offset = gid - allIds.min
      val add = allIds.max + 1 - allIds.min
      gid = gid + add
      offset
    })
  }

  private def squashPoints(pts: Seq[HAssertBundle])(implicit p: Parameters): Seq[HAssertBundle] = {
    val hwaP = p(HardwareAssertionKey)
    pts.foreach(pp => require(pp.node.level == 0))
    pts.groupBy(_.node.desc.head.identity).toSeq.sortBy(_._1).map({case(_, ns) =>
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
      val newNode = HAssertNode(desc = Seq(ns.head.node.desc.head.withId(gid)), name = ns.head.node.name)
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

  /** Checks for a condition to be valid in the circuit at rising clock edge
   * when not in reset. If the condition evaluates to false, the circuit
   * simulation stops with an error. The assert id and user bits will be
   * output to the module interface.
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param hardDesc required static error description used by silicon debug metadata
   * @param softDesc optional runtime context printed only by the simulation assertion
   */
  def apply(cond: Bool, hardDesc: String, softDesc: Printable)(implicit p: Parameters, s: SourceInfo): Unit =
    applyImpl(cond, hardDesc, Some(softDesc))(p, s)

  def apply(cond: Bool, hardDesc: String)(implicit p: Parameters, s: SourceInfo): Unit =
    applyImpl(cond, hardDesc, None)(p, s)

  private def applyImpl(cond: Bool, hardDesc: String, softDesc: Option[Printable])(
    implicit p: Parameters,
    s: SourceInfo
  ): Unit = {
    val metadata = sourceDesc(hardDesc, s)
    val assertCond = cond
    assert(assertCond, simulationDesc(metadata, softDesc))(s)
    val hwaP = p(HardwareAssertionKey)
    if(hwaP.enable) {
      val identity = s"${metadata.hardDesc}:${metadata.location}"
      val hashCode = s"${identity.hashCode}"
      if(!hashToCountMap.contains(hashCode)) {
        hashToCountMap.addOne((hashCode, 0))
      }
      hashToCountMap(hashCode) = hashToCountMap(hashCode) + 1
      val pcode = s"${hashCode}_${hashToCountMap(hashCode) - 1}"
      val node = HAssertNode(desc = Seq(metadata), level = 0, point = true, metadata.hardDesc)
      val thisCond = EscapedWire(new HAssertBundle(node))
      thisCond.cond.get := !assertCond
      thisCond.suggestName(s"hwa_$pcode")
      SpecialWireInit(s, thisCond.cond.get, 0, prepend = true)
      hwaSeq = hwaSeq :+ thisCond
    }
  }

  def withEn(cond: Bool, en: Bool, hardDesc: String, softDesc: Printable)(implicit p: Parameters, s: SourceInfo): Unit =
    apply(Mux(en, cond, true.B), hardDesc, softDesc)(p, s)

  def withEn(cond: Bool, en: Bool, hardDesc: String)(implicit p: Parameters, s: SourceInfo): Unit =
    apply(Mux(en, cond, true.B), hardDesc)(p, s)

  def checkTimeout(clear: Bool, timeout: Int, hardDesc: String, softDesc: Printable)(
    implicit p: Parameters,
    s: SourceInfo
  ): Bool = checkTimeoutImpl(clear, timeout, hardDesc, Some(softDesc))(p, s)

  def checkTimeout(clear: Bool, timeout: Int, hardDesc: String)(implicit p: Parameters, s: SourceInfo): Bool =
    checkTimeoutImpl(clear, timeout, hardDesc, None)(p, s)

  private def checkTimeoutImpl(clear: Bool, timeout: Int, hardDesc: String, softDesc: Option[Printable])(
    implicit p: Parameters,
    s: SourceInfo
  ): Bool = {
    val to_val = 0x1L << log2Ceil(3_000_000)
    require(timeout <= to_val)
    val to_cnt = Reg(UInt(log2Ceil(to_val + 1).W))
    when(clear || to_cnt < to_val.U) {
      to_cnt := Mux(clear, 0.U, to_cnt + 1.U)
    }
    val eda_err = to_cnt >= timeout.U
    val hwa_err = to_cnt >= to_val.U
    val metadata = sourceDesc(hardDesc, s)
    assert(!eda_err, simulationDesc(metadata, softDesc))
    softDesc match {
      case Some(desc) => apply(!hwa_err, hardDesc, desc)(p, s)
      case None => apply(!hwa_err, hardDesc)(p, s)
    }
    eda_err
  }

  def placePipe(level: Int, moduleTop: Boolean = false, name:String = "")(
    implicit p: Parameters
  ): Option[Seq[HAssertBundle]] = {
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
        val hwa_n = HAssertNode(desc = dedupDesc(cs.flatMap(_.node.desc)), level = level, name = name)
        val hwa_out = Wire(new HAssertBundle(hwa_n))
        if(cs.size > 1) {
          val hwa_arb = Module(new ResetRRArbiter(gen = UInt(width.W), n = cs.size))
          val hwa_q = Module(new FastQueue(gen = UInt(width.W), size = 2))
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
        importedLayoutOffsets.clear()
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
      val offset = importOffset(_buses.toSeq)
      for(i <- _buses.indices) yield {
        val _nd = _buses(i).node.desc.map(e => e.withId(e.id + offset))
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
        val allIds = children.flatMap(_.node.desc.map(_.id))
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

  private def jsonString(value: String): String = {
    val escaped = value.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c => c.toString
    }
    s"\"$escaped\""
  }

  private def renderManifest(
    producer: String,
    runtimeSourceTypes: Seq[String],
    idWidth: Int,
    desc: Seq[HAssertDesc]
  ): String = {
    val assertions = dedupDesc(desc)
    val records = assertions.map { entry =>
      val idHex = s"0x${entry.id.toHexString.reverse.padTo((idWidth + 3) / 4, '0').reverse}"
      s"""    {
         |      "id": ${entry.id},
         |      "id_hex": ${jsonString(idHex)},
         |      "hardware_message": ${jsonString(entry.hardDesc)}
         |    }""".stripMargin
    }.mkString(",\n")
    val sources = runtimeSourceTypes.map(jsonString).mkString(", ")
    s"""{
       |  "schema": "zhujiang-hardware-assertions-v1",
       |  "producer": ${jsonString(producer)},
       |  "runtime_source_types": [$sources],
       |  "id_width": $idWidth,
       |  "assertion_count": ${assertions.size},
       |  "assertions": [
       |$records
       |  ]
       |}
       |""".stripMargin
  }

  private def renderCatalog: String = {
    if(releasedManifests.isEmpty) {
      return renderedCatalog.getOrElse(throw new IllegalStateException("no HWA manifests were released"))
    }
    val refs = releasedManifests.values.toSeq.sortBy(_.producer).map { reference =>
      val sources = reference.runtimeSourceTypes.map(jsonString).mkString(", ")
      s"""    {
         |      "producer": ${jsonString(reference.producer)},
         |      "runtime_source_types": [$sources],
         |      "file": ${jsonString(reference.file)},
         |      "assertion_count": ${reference.assertionCount},
         |      "id_width": ${reference.idWidth}
         |    }""".stripMargin
    }.mkString(",\n")
    val catalog = s"""{
       |  "schema": "zhujiang-hardware-assertion-catalog-v1",
       |  "manifests": [
       |$refs
       |  ]
       |}
       |""".stripMargin
    releasedManifests.clear()
    renderedCatalog = Some(catalog)
    catalog
  }

  def release(as: Option[Seq[HAssertBundle]], dir: String, pfx: String, runtimeSourceTypes: Seq[String])(
    implicit p: Parameters
  ): Unit = {
    require(pfx.nonEmpty, "HAssert release producer must be non-empty")
    require(runtimeSourceTypes.nonEmpty, s"HAssert release $pfx must declare at least one runtime source type")
    as.foreach(_.foreach(a => {
      hwaSeq = Nil
      gid = 0
      val assertions = dedupDesc(a.node.desc)
      val idWidth = p(HardwareAssertionKey).maxInfoBits
      val manifestFile = s"$pfx-hardware-assertions.json"
      val reference = ManifestRef(pfx, runtimeSourceTypes.distinct.sorted, manifestFile, assertions.size, idWidth)
      releasedManifests.get(pfx).foreach { existing =>
        require(existing == reference, s"conflicting HWA manifest for producer $pfx")
      }
      releasedManifests(pfx) = reference
      renderedCatalog = None
      FileRegisters.add(
        dir,
        manifestFile,
        renderManifest(pfx, reference.runtimeSourceTypes, idWidth, assertions),
        dontCarePrefix = true
      )
      val text = assertions.map(d => f"${d.id}%d | 0x${d.id}%04x | ${d.hardDesc}").mkString("\n") + "\n"
      FileRegisters.add(dir, s"${pfx}_hardware_assertion.txt", text, dontCarePrefix = true)
      if(!FileRegisters.contains("hardware-assertion-catalog.json")) {
        FileRegisters.add(dir, "hardware-assertion-catalog.json", renderCatalog, dontCarePrefix = true)
      }
    }))
  }
}
