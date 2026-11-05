package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{MixedVec, UIntToOH, log2Ceil}
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import xs.utils.GlobalData
import xs.utils.verilog.{SvUIntToOH, SvUIntToOHImpl, VlogRegistry}

import scala.util.Random

private[test] class SvUIntToOHCorePort(size: Int) extends Bundle {
  val in = Input(UInt(math.max(1, log2Ceil(size)).W))
  val results = Output(Vec(4, UInt(size.W)))
}

private[test] class SvUIntToOHCoreBank(sizes: Seq[Int]) extends RawModule {
  val io = IO(MixedVec(sizes.map(new SvUIntToOHCorePort(_))))
  sizes.zip(io).foreach { case (size, port) =>
    val core = Module(if (size == 2) new SvUIntToOHImpl() else new SvUIntToOHImpl(size))
    val wrapper = Module(if (size == 2) new SvUIntToOH() else new SvUIntToOH(size))
    core.io.in := port.in
    wrapper.io.in := port.in
    Seq(core.io.out, wrapper.io.out, SvUIntToOH(port.in, size), UIntToOH(port.in, size))
      .zip(port.results).foreach { case (value, out) =>
        require(value.getWidth == size)
        out := value
      }
  }
}

private[test] case class UIntToOHCase(inputWidth: Int, width: Option[Int] = None) {
  val outputWidth: Int = width.getOrElse(1 << inputWidth)
}

private[test] class SvUIntToOHApiPort(c: UIntToOHCase) extends Bundle {
  val in = Input(UInt(math.max(1, c.inputWidth).W))
  val actual = Output(UInt(math.max(1, c.outputWidth).W))
  val reference = Output(UInt(math.max(1, c.outputWidth).W))
}

private[test] class SvUIntToOHApiBank(cases: Seq[UIntToOHCase]) extends RawModule {
  val io = IO(MixedVec(cases.map(new SvUIntToOHApiPort(_))))
  cases.zip(io).foreach { case (c, port) =>
    val in = if (c.inputWidth == 0) 0.U(0.W) else port.in
    val actual = c.width.map(SvUIntToOH(in, _)).getOrElse(SvUIntToOH(in))
    val reference = c.width.map(UIntToOH(in, _)).getOrElse(UIntToOH(in))
    require(actual.getWidth == c.outputWidth && reference.getWidth == c.outputWidth)
    port.actual := actual
    port.reference := reference
  }
}

class SvUIntToOHSpec extends SimTestSpec {
  private def expected(value: BigInt, width: Int): BigInt = {
    if (width == 0) BigInt(0)
    else if (width == 1) BigInt(1)
    else {
      val index = (value & ((BigInt(1) << log2Ceil(width)) - 1)).toInt
      if (index < width) BigInt(1) << index else BigInt(0)
    }
  }

  describe("SvUIntToOH") {
    it("exhaustively decodes all indices in raw cores, wrappers and factories including unused indices") {
      val sizes = (1 to 8) ++ Seq(15, 16, 17, 31, 32, 33, 63, 64, 65, 129)
      simulateRaw(new SvUIntToOHCoreBank(sizes)) { dut =>
        sizes.zip(dut.io).foreach { case (size, port) =>
          for (index <- 0 until (1 << math.max(1, log2Ceil(size)))) {
            port.in.poke(index.U)
            port.results.foreach(_.expect(expected(index, size).U, s"size=$size index=$index"))
          }
        }
      }
    }

    it("matches native inferred widths, explicit truncation, zero extension and zero-width inputs") {
      val cases = (0 to 8).map(UIntToOHCase(_)) ++
        (for (inputWidth <- Seq(0, 1, 2, 8, 65); width <- Seq(0, 1, 2, 3, 5, 17, 65))
          yield UIntToOHCase(inputWidth, Some(width)))
      val seed = testSeed(0x0170L)
      val cycles = testCycles(32)
      val random = new Random(seed)
      info(s"seed=$seed cycles=$cycles")
      simulateRaw(new SvUIntToOHApiBank(cases)) { dut =>
        cases.zip(dut.io).foreach { case (c, port) =>
          val values = if (c.inputWidth <= 8) BigInt(0) until (BigInt(1) << c.inputWidth)
          else Seq(BigInt(0), (BigInt(1) << c.inputWidth) - 1) ++
            (0 until c.inputWidth).map(BigInt(1) << _) ++ Seq.fill(cycles)(BigInt(c.inputWidth, random))
          values.foreach { value =>
            val clue = s"seed=$seed case=$c value=$value"
            port.in.poke(value.U)
            val result = expected(value, c.outputWidth)
            port.actual.expect(result.U, clue)
            port.reference.expect(result.U, clue)
          }
        }
      }
    }

    it("supports Bool inputs and explicit output width before input width inference") {
      simulateRaw(new RawModule {
        val in = IO(Input(UInt(8.W)))
        val flag = IO(Input(Bool()))
        val out = IO(Output(Vec(5, UInt(5.W))))
        val unknown = Wire(UInt())
        out(0) := SvUIntToOH(unknown, 5)
        out(1) := UIntToOH(unknown, 5)
        unknown := in
        out(2) := SvUIntToOH(flag, 5)
        out(3) := SvUIntToOH(flag)
        // A UInt wire also lets the native helper pad a Bool input.
        val uintFlag = Wire(UInt(1.W))
        uintFlag := flag
        out(4) := UIntToOH(uintFlag, 5)
      }) { dut =>
        for (value <- 0 until 256; flag <- Seq(false, true)) {
          dut.in.poke(value.U)
          dut.flag.poke(flag.B)
          dut.out.take(2).foreach(_.expect(expected(value, 5).U))
          dut.out.drop(2).foreach(_.expect((if (flag) 2 else 1).U))
        }
      }
    }

    it("returns native constants without an SV core for output widths zero and one") {
      val rtl = ChiselStage.emitSystemVerilog(new RawModule {
        val in = IO(Input(UInt(8.W)))
        val out = IO(Output(Vec(3, UInt(1.W))))
        val empty = SvUIntToOH(in, 0)
        val singleton = SvUIntToOH(in, 1)
        val zeroIndex = SvUIntToOH(0.U(0.W))
        require(empty.getWidth == 0 && empty.litValue == 0)
        require(singleton.getWidth == 1 && singleton.litValue == 1)
        require(zeroIndex.getWidth == 1 && zeroIndex.litValue == 1)
        out := VecInit(Seq(empty, singleton, zeroIndex))
      })
      assert("(?m)^\\s*module\\s+SvUIntToOH\\b".r.findFirstIn(rtl).isEmpty)
    }

    it("inlines wrappers, deduplicates parameterized cores and preserves global prefixes") {
      GlobalData.synchronized {
        val previous = GlobalData.prefix
        try for (prefix <- Seq("", "soc_")) {
          GlobalData.prefix = prefix
          val name = prefix + "SvUIntToOH"
          val sources = new VlogRegistry().sources(SvUIntToOHImpl)
          assert(sources.map(_.moduleName) == Seq(name))
          val annotations = ChiselStage.elaborate(new SvUIntToOHCoreBank(Seq(2, 3, 3, 65)))
            .annotations.collect { case a: BlackBoxInlineAnno => a.name }.toSeq
          assert(annotations == Seq(name + ".sv"))
          val rtl = ChiselStage.emitSystemVerilog(new SvUIntToOHCoreBank(Seq(2, 3, 3, 65)))
          val modules = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
          assert(modules.sorted == Seq("SvUIntToOHCoreBank", name).sorted)
          assert(rtl.contains("parameter integer OUTPUT_SIZE = 2"))
          assert(rtl.contains(".OUTPUT_SIZE(65)"))
        } finally GlobalData.prefix = previous
      }
    }

    it("rejects invalid dimensions, negative output widths, unknown inference and width overflow") {
      for (size <- Seq(0, -1); raw <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (raw) { val bad = Module(new SvUIntToOHImpl(size)) }
            else { val bad = Module(new SvUIntToOH(size)) }
          })
        }
      }
      for (kind <- Seq("negative", "unknown", "overflow")) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val in = IO(Input(UInt(8.W)))
            val bad = kind match {
              case "negative" => SvUIntToOH(in, -1)
              case "unknown" => SvUIntToOH(Wire(UInt()))
              case _ => SvUIntToOH(IO(Input(UInt(31.W))))
            }
          })
        }
      }
    }

    it("rejects unbound inputs in both overloads including constant bypasses") {
      for (width <- Seq(None, Some(0), Some(1), Some(3))) {
        intercept[ExpectedHardwareException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val in = UInt(2.W)
            val bad = width.map(SvUIntToOH(in, _)).getOrElse(SvUIntToOH(in))
          })
        }
      }
    }
  }
}
