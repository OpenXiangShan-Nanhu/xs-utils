package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.MixedVec
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import xs.utils.GlobalData
import xs.utils.verilog._

import scala.util.Random

private[test] class MaskToOHPort(size: Int) extends Bundle {
  val in = Input(UInt(size.W))
  val low = Output(Vec(3, UInt(size.W)))
  val high = Output(Vec(3, UInt(size.W)))
}

private[test] class MaskToOHBank(sizes: Seq[Int]) extends RawModule {
  val io = IO(MixedVec(sizes.map(new MaskToOHPort(_))))
  sizes.zip(io).foreach { case (size, port) =>
    for ((high, results) <- Seq(false -> port.low, true -> port.high)) {
      val core = Module(if (size == 8 && !high) new SvMaskToOHImpl() else new SvMaskToOHImpl(size, high))
      val wrapper = Module(if (size == 8 && !high) new SvMaskToOH() else new SvMaskToOH(size, high))
      core.io.in := port.in
      wrapper.io.in := port.in
      val factory = if (high) SvMaskToOH(port.in, highMask = true) else SvMaskToOH(port.in)
      Seq(core.io.out, wrapper.io.out, factory).zip(results).foreach { case (oh, out) =>
        require(oh.getWidth == size)
        out := oh
      }
    }
  }
}

class SvMaskToOHSpec extends SimTestSpec {
  // Independent reference: find the extreme asserted mask bit and move one position inward.
  private def expected(mask: BigInt, size: Int, high: Boolean): BigInt = {
    val bits = (0 until size).filter(mask.testBit)
    if (bits.isEmpty) BigInt(0)
    else {
      val position = (if (high) bits.min - 1 else bits.max + 1)
      if (position < 0 || position >= size) BigInt(0) else BigInt(1) << position
    }
  }

  private def exercise(sizes: Seq[Int], exhaustive: Boolean): Unit = {
    val seed = testSeed(0x5a05L)
    val cycles = testCycles(64)
    val random = new Random(seed)
    info(s"seed=$seed cycles=$cycles")
    simulateRaw(new MaskToOHBank(sizes)) { dut =>
      sizes.zip(dut.io).foreach { case (size, port) =>
        val all = (BigInt(1) << size) - 1
        val boundaries = (0 until size).map(bit => (BigInt(1) << bit) - 1) ++
          (0 until size).map(bit => all ^ ((BigInt(1) << (bit + 1)) - 1))
        val values = if (exhaustive) (BigInt(0) to all).toSeq else
          Seq(BigInt(0), all) ++ boundaries ++ Seq.fill(cycles)(BigInt(size, random))
        for (value <- values) {
          val clue = s"size=$size mask=0x${value.toString(16)} seed=$seed"
          port.in.poke(value.U)
          port.low.foreach(_.expect(expected(value, size, high = false).U, clue))
          port.high.foreach(_.expect(expected(value, size, high = true).U, clue))
        }
      }
    }
  }

  describe("SvMaskToOH") {
    it("exhaustively decodes both mask directions, empty/full masks, and irregular masks at widths one through eight") {
      exercise(1 to 8, exhaustive = true)
    }

    it("checks contiguous boundaries and random masks across bits 31 and 63") {
      exercise(Seq(9, 17, 31, 32, 33, 63, 64, 65, 129), exhaustive = false)
    }

    it("round-trips one-hot inputs through SvOHToMask except for the empty-mask endpoints") {
      val sizes = Seq(1, 2, 3, 8, 17, 65)
      simulateRaw(new RawModule {
        val io = IO(MixedVec(sizes.map(new MaskToOHPort(_))))
        io.foreach { port =>
          port.low(0) := SvOHToMask(port.in)
          port.high(0) := SvOHToMask(port.in, highMask = true)
          port.low(1) := SvMaskToOH(port.low(0))
          port.high(1) := SvMaskToOH(port.high(0), highMask = true)
          port.low(2) := SvOHToMask(port.low(1))
          port.high(2) := SvOHToMask(port.high(1), highMask = true)
        }
      }) { dut =>
        sizes.zip(dut.io).foreach { case (size, port) =>
          for (bit <- -1 until size) {
            val oh = if (bit < 0) BigInt(0) else BigInt(1) << bit
            val lowMask = if (bit < 0) BigInt(0) else oh - 1
            val highMask = if (bit < 0) BigInt(0) else ((BigInt(1) << size) - 1) ^ ((oh << 1) - 1)
            port.in.poke(oh.U)
            port.low(0).expect(lowMask.U)
            port.high(0).expect(highMask.U)
            port.low(1).expect((if (bit <= 0) BigInt(0) else oh).U)
            port.high(1).expect((if (bit == size - 1) BigInt(0) else oh).U)
            port.low(2).expect(lowMask.U)
            port.high(2).expect(highMask.U)
          }
        }
      }
    }

    it("accepts signed bit patterns and Bool inputs without changing their widths") {
      simulateRaw(new RawModule {
        val in = IO(Input(SInt(5.W)))
        val flag = IO(Input(Bool()))
        val low = IO(Output(UInt(5.W)))
        val high = IO(Output(UInt(5.W)))
        val single = IO(Output(Vec(2, UInt(1.W))))
        low := SvMaskToOH(in)
        high := SvMaskToOH(in, highMask = true)
        single(0) := SvMaskToOH(flag)
        single(1) := SvMaskToOH(flag, highMask = true)
      }) { dut =>
        for (value <- -16 until 16; flag <- Seq(false, true)) {
          dut.in.poke(value.S)
          dut.flag.poke(flag.B)
          dut.low.expect(expected(BigInt(value & 31), 5, high = false).U)
          dut.high.expect(expected(BigInt(value & 31), 5, high = true).U)
          dut.single.foreach(_.expect(0.U))
        }
      }
    }

    it("inlines wrappers and deduplicates the prefixed encoder dependencies across directions and widths") {
      GlobalData.synchronized {
        val saved = GlobalData.prefix
        try for (prefix <- Seq("", "soc_")) {
          GlobalData.prefix = prefix
          val expectedNames = Seq("SvPrefixReduceOR", "SvPriorityEncoder1H", "SvMaskToOH").map(prefix + _)
          val registry = new VlogRegistry()
          val sources = registry.sources(SvMaskToOHImpl)
          assert(sources.map(_.moduleName) == expectedNames)
          assert(sources.last.text.contains(
            s"${prefix}SvPriorityEncoder1H #(.INPUT_SIZE(INPUT_SIZE), .LOW_FIRST(int'(HIGH_MASK != 0))) u_edge"))
          assert(sources.last.text.contains("assign io_out = edge_oh >> 1;"))
          assert(sources.last.text.contains("assign io_out = edge_oh << 1;"))
          assert(!sources.last.text.contains("logic scan"))
          val roots = Seq(SvOHToMaskImpl, SvMaskToOHImpl, SvPriorityEncoder1HImpl, SvPrefixReduceImpl(0))
          val sharedNames = expectedNames :+ s"${prefix}SvOHToMask"
          for (ordered <- Seq(roots, roots.reverse)) {
            assert(registry.sources(ordered).map(_.moduleName).sorted == sharedNames.sorted)
          }
          val annotations = ChiselStage.elaborate(new MaskToOHBank(Seq(1, 8, 65))).annotations.collect {
            case a: BlackBoxInlineAnno => a.name
          }.toSeq
          assert(annotations == (expectedNames.last +: expectedNames.init).map(_ + ".sv"))
          val rtl = ChiselStage.emitSystemVerilog(new MaskToOHBank(Seq(1, 8, 65)))
          val names = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
          assert(names.sorted == (expectedNames :+ "MaskToOHBank").sorted)
          assert(rtl.contains(".HIGH_MASK(0)") && rtl.contains(".HIGH_MASK(1)"))
        } finally GlobalData.prefix = saved
      }
    }

    it("rejects nonpositive sizes, unknown widths and unbound inputs") {
      for (size <- Seq(0, -1); ext <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (ext) { val bad = Module(new SvMaskToOHImpl(size)) }
            else { val bad = Module(new SvMaskToOH(size)) }
          })
        }
      }
      for (unknown <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val in = if (unknown) Wire(UInt()) else 0.U(0.W)
            val bad = SvMaskToOH(in)
          })
        }
      }
      for (gen <- Seq(UInt(8.W), SInt(8.W), Bool())) {
        intercept[ExpectedHardwareException] {
          ChiselStage.emitCHIRRTL(new RawModule { val bad = SvMaskToOH(gen) })
        }
      }
    }
  }
}
