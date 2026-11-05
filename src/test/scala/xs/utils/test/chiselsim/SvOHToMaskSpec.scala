package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.MixedVec
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import xs.utils.GlobalData
import xs.utils.verilog._

import scala.util.Random

private[test] class OHToMaskPort(size: Int) extends Bundle {
  val in = Input(UInt(size.W))
  val low = Output(Vec(3, UInt(size.W)))
  val high = Output(Vec(3, UInt(size.W)))
}

private[test] class OHToMaskBank(sizes: Seq[Int]) extends RawModule {
  val io = IO(MixedVec(sizes.map(new OHToMaskPort(_))))
  sizes.zip(io).foreach { case (size, port) =>
    for ((high, results) <- Seq(false -> port.low, true -> port.high)) {
      val core = Module(if (size == 8 && !high) new SvOHToMaskImpl() else new SvOHToMaskImpl(size, high))
      val wrapper = Module(if (size == 8 && !high) new SvOHToMask() else new SvOHToMask(size, high))
      core.io.in := port.in
      wrapper.io.in := port.in
      val factory = if (high) SvOHToMask(port.in, highMask = true) else SvOHToMask(port.in)
      Seq(core.io.out, wrapper.io.out, factory).zip(results).foreach { case (mask, out) =>
        require(mask.getWidth == size)
        out := mask
      }
    }
  }
}

class SvOHToMaskSpec extends SimTestSpec {
  // Independent arithmetic model: OR the strict-side mask of each asserted bit.
  private def expected(value: BigInt, size: Int, high: Boolean): BigInt = {
    val all = (BigInt(1) << size) - 1
    (0 until size).filter(value.testBit).foldLeft(BigInt(0)) { (mask, bit) =>
      mask | (if (high) all ^ ((BigInt(1) << (bit + 1)) - 1) else (BigInt(1) << bit) - 1)
    }
  }

  private def exercise(sizes: Seq[Int], exhaustive: Boolean): Unit = {
    val seed = testSeed(0x0a5cL)
    val cycles = testCycles(64)
    val random = new Random(seed)
    info(s"seed=$seed cycles=$cycles")
    simulateRaw(new OHToMaskBank(sizes)) { dut =>
      sizes.zip(dut.io).foreach { case (size, port) =>
        val all = (BigInt(1) << size) - 1
        val values = if (exhaustive) (BigInt(0) to all).toSeq else
          Seq(BigInt(0), all) ++ (0 until size).map(BigInt(1) << _) ++
            Seq.fill(cycles)(BigInt(size, random)) ++ Seq(BigInt(0))
        for (value <- values) {
          val clue = s"size=$size input=0x${value.toString(16)} seed=$seed"
          port.in.poke(value.U)
          port.low.foreach(_.expect(expected(value, size, high = false).U, clue))
          port.high.foreach(_.expect(expected(value, size, high = true).U, clue))
        }
      }
    }
  }

  describe("SvOHToMask") {
    it("exhaustively checks both directions, strict boundaries, zero and multiple hits for widths one through eight") {
      exercise(1 to 8, exhaustive = true)
    }

    it("checks every one-hot position and random masks across bits 31 and 63") {
      exercise(Seq(9, 17, 31, 32, 33, 63, 64, 65, 129), exhaustive = false)
    }

    it("accepts signed bit patterns and Bool inputs without changing their widths") {
      simulateRaw(new RawModule {
        val in = IO(Input(SInt(5.W)))
        val flag = IO(Input(Bool()))
        val low = IO(Output(UInt(5.W)))
        val high = IO(Output(UInt(5.W)))
        val single = IO(Output(Vec(2, UInt(1.W))))
        low := SvOHToMask(in)
        high := SvOHToMask(in, highMask = true)
        single(0) := SvOHToMask(flag)
        single(1) := SvOHToMask(flag, highMask = true)
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

    it("inlines wrappers and registers one prefixed template for both directions and multiple widths") {
      GlobalData.synchronized {
        val saved = GlobalData.prefix
        try for (prefix <- Seq("", "soc_")) {
          GlobalData.prefix = prefix
          val expected = prefix + "SvOHToMask"
          val annotations = ChiselStage.elaborate(new OHToMaskBank(Seq(1, 8, 65))).annotations.collect {
            case a: BlackBoxInlineAnno => a.name
          }.toSeq
          assert(annotations == Seq(expected + ".sv"))
          val rtl = ChiselStage.emitSystemVerilog(new OHToMaskBank(Seq(1, 8, 65)))
          val names = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
          assert(names.sorted == Seq("OHToMaskBank", expected).sorted)
          assert(rtl.contains(".HIGH_MASK(0)") && rtl.contains(".HIGH_MASK(1)"))
        } finally GlobalData.prefix = saved
      }
    }

    it("rejects nonpositive sizes, unknown widths and unbound inputs") {
      for (size <- Seq(0, -1); ext <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (ext) { val bad = Module(new SvOHToMaskImpl(size)) }
            else { val bad = Module(new SvOHToMask(size)) }
          })
        }
      }
      for (unknown <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val in = if (unknown) Wire(UInt()) else 0.U(0.W)
            val bad = SvOHToMask(in)
          })
        }
      }
      for (gen <- Seq(UInt(8.W), SInt(8.W), Bool())) {
        intercept[ExpectedHardwareException] {
          ChiselStage.emitCHIRRTL(new RawModule { val bad = SvOHToMask(gen) })
        }
      }
    }
  }
}
