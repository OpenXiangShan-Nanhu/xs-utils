package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{MixedVec, OHToUInt, log2Ceil}
import circt.stage.ChiselStage
import xs.utils.test.WrapperTestBit
import xs.utils.verilog.{SvOHToUInt, SvOHToUIntImpl}

import scala.util.Random

private[test] class SvOHToUIntPort(size: Int) extends Bundle {
  val in = Input(Vec(size, new WrapperTestBit))
  val results = Output(Vec(8, UInt(math.max(1, log2Ceil(size)).W)))
  val low = Output(UInt(math.max(1, log2Ceil(math.max(1, size - 1))).W))
  val lowReference = Output(UInt(math.max(1, log2Ceil(math.max(1, size - 1))).W))
}

private[test] class SvOHToUIntBank(sizes: Seq[Int]) extends Module {
  val io = IO(MixedVec(sizes.map(new SvOHToUIntPort(_))))
  sizes.zip(io).foreach { case (size, port) =>
    val packed = port.in.asUInt
    val ext = Module(if (size == 2) new SvOHToUIntImpl() else new SvOHToUIntImpl(size))
    val wrapper = Module(if (size == 2) new SvOHToUInt() else new SvOHToUInt(size))
    ext.io_in := packed
    wrapper.io.in := packed
    val outputs: Seq[UInt] = Seq(ext.io_out, wrapper.io.out, SvOHToUInt(packed),
      SvOHToUInt(packed.asSInt), SvOHToUInt(packed.asBools), SvOHToUInt(VecInit(packed.asBools)),
      SvOHToUInt(packed, size), OHToUInt(packed))
    outputs.zip(port.results).foreach { case (value, out) =>
      require(value.getWidth == math.max(1, log2Ceil(size)))
      out := value
    }
    port.low := SvOHToUInt(packed, size - 1)
    port.lowReference := OHToUInt(packed, size - 1)
  }
}

class SvOHToUIntSpec extends SimTestSpec {
  private def exercise(sizes: Seq[Int], exhaustive: Boolean): Unit = {
    val seed = testSeed(0x0f701L)
    val cycles = testCycles(128)
    val random = new Random(seed)
    info(s"seed=$seed cycles=$cycles exhaustive=$exhaustive sizes=$sizes")
    simulate(new SvOHToUIntBank(sizes)) { dut =>
      sizes.zipWithIndex.foreach { case (size, index) =>
        val all = (BigInt(1) << size) - 1
        val values = if (exhaustive) (BigInt(0) to all).toSeq else
          Seq(BigInt(0), all) ++ (0 until size).map(BigInt(1) << _) ++
            Seq.fill(cycles)(BigInt(size, random)) ++ Seq(BigInt(0))
        values.foreach { value =>
          // Independent index-OR model also checks the local native implementation
          // on zero/multi-hot inputs, beyond OHToUInt's public one-hot guarantee.
          def expected(width: Int): BigInt =
            (0 until width).filter(value.testBit).foldLeft(BigInt(0))((acc, bit) => acc | BigInt(bit))
          val port = dut.io(index)
          val clue = s"seed=$seed size=$size value=0x${value.toString(16)}"
          pokePayload(port.in, value)
          port.results.foreach(_.expect(expected(size).U, clue))
          port.low.expect(expected(size - 1).U, clue)
          port.lowReference.expect(expected(size - 1).U, clue)
        }
      }
    }
  }

  describe("SvOHToUInt") {
    it("matches all native overloads exhaustively for widths one through eight, including defaults") {
      exercise(1 to 8, exhaustive = true)
    }

    it("encodes every one-hot position and random patterns across machine-word boundaries") {
      exercise(Seq(9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 129), exhaustive = false)
    }

    it("matches empty and singleton API results and bypasses the SV core") {
      val rtl = ChiselStage.emitSystemVerilog(new RawModule {
        val in = IO(Input(Bool()))
        val out = IO(Output(Vec(8, UInt(1.W))))
        val emptyVec = Wire(Vec(0, Bool()))
        val pairs = Seq(
          (SvOHToUInt(Seq.empty[Bool]), OHToUInt(Seq.empty[Bool])),
          (SvOHToUInt(emptyVec), OHToUInt(emptyVec)),
          (SvOHToUInt(0.U(0.W)), OHToUInt(0.U(0.W))),
          (SvOHToUInt(in), OHToUInt(in)),
          (SvOHToUInt(Seq(in)), OHToUInt(Seq(in))),
          (SvOHToUInt(VecInit(Seq(in))), OHToUInt(VecInit(Seq(in)))),
          (SvOHToUInt(in, 0), OHToUInt(in, 0)),
          (SvOHToUInt(0.U(0.W), 1), OHToUInt(0.U(0.W), 1)))
        pairs.zip(out).foreach { case ((actual, reference), result) =>
          require(actual.getWidth == 1 && reference.getWidth == 1)
          require(actual.litValue == 0 && reference.litValue == 0)
          result := actual
        }
      })
      assert(!rtl.contains("module SvOHToUInt #("))
    }

    it("supports explicit width with an input whose width is inferred later") {
      simulate(new Module {
        val in = IO(Input(UInt(8.W)))
        val out = IO(Output(UInt(2.W)))
        val unknown = Wire(UInt())
        out := SvOHToUInt(unknown, 3)
        unknown := in
      }) { dut =>
        Seq(0 -> 0, 1 -> 0, 2 -> 1, 4 -> 2, 7 -> 3, 128 -> 0, 132 -> 2).foreach {
          case (value, expected) =>
            dut.in.poke(value.U)
            dut.out.expect(expected.U)
        }
      }
    }

    it("inlines wrappers and deduplicates the parameterized SV template") {
      val rtl = ChiselStage.emitSystemVerilog(new SvOHToUIntBank(Seq(2, 3, 3, 17)))
      val names = """(?m)^\s*module\s+(SvOHToUInt(?:_[0-9]+)?)\b""".r
        .findAllMatchIn(rtl).map(_.group(1)).toSeq
      assert(names == Seq("SvOHToUInt"), names.mkString(", "))
      assert(rtl.contains("parameter integer INPUT_SIZE = 2"))
      assert(rtl.contains(".INPUT_SIZE(17)"))
    }

    it("rejects invalid dimensions, negative widths, oversized slices and implicit unknown widths") {
      for (size <- Seq(0, -1); ext <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (ext) { val bad = Module(new SvOHToUIntImpl(size)) }
            else { val bad = Module(new SvOHToUInt(size)) }
          })
        }
      }
      for (kind <- Seq("negative", "oversized", "unknown")) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val in = IO(Input(UInt(2.W)))
            val bad = kind match {
              case "negative" => SvOHToUInt(in, -1)
              case "oversized" => SvOHToUInt(in, 3)
              case _ => SvOHToUInt(IO(Input(UInt())))
            }
          })
        }
      }
    }

    it("rejects unbound types for packed, sequence and vector overloads including bypasses") {
      for (size <- Seq(0, 1, 2); kind <- Seq("packed", "explicit", "seq", "vec")
           if kind != "seq" || size > 0) {
        intercept[ExpectedHardwareException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val bad = kind match {
              case "packed" => SvOHToUInt(UInt(size.W))
              case "explicit" => SvOHToUInt(UInt(size.W), size)
              case "seq" => SvOHToUInt(Seq.fill(size)(Bool()))
              case _ => SvOHToUInt(Vec(size, Bool()))
            }
          })
        }
      }
    }
  }
}
