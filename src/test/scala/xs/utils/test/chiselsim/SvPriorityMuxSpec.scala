package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{MixedVec, PriorityMux, Reverse}
import circt.stage.ChiselStage
import xs.utils.test.{WrapperTestPacket, WrapperTestPayload}
import xs.utils.verilog.{SvPriorityMux, SvPriorityMuxImpl => SvPriorityMuxExt}

import scala.util.Random

private[test] case class SvPriorityMuxCase(width: Int, size: Int, payload: String = "bundle", lowFirst: Boolean = true) {
  def gen: Data = WrapperTestPayload(width, payload)
}

private[test] class SvPriorityMuxPort(c: SvPriorityMuxCase) extends Bundle {
  val sel = Input(UInt(c.size.W))
  val in = Input(Vec(c.size, c.gen))
  val direct = Output(c.gen)
  val explicit = Output(c.gen)
  val pairs = Output(c.gen)
  val iterablePairs = Output(c.gen)
  val seqSel = Output(c.gen)
  val bitsSel = Output(c.gen)
  val sintSel = Output(c.gen)
  val boolSingle = Output(c.gen)
  val nativeSeq = Output(c.gen)
  val nativePairs = Output(c.gen)
  val nativeSint = Output(c.gen)
  val nativeBool = Output(c.gen)
}

private[test] class SvPriorityMuxBank(cases: Seq[SvPriorityMuxCase]) extends Module {
  val io = IO(MixedVec(cases.map(new SvPriorityMuxPort(_))))
  cases.zip(io).foreach { case (c, port) =>
    val ext = Module(if (c.lowFirst) new SvPriorityMuxExt(c.width, c.size)
      else new SvPriorityMuxExt(c.width, c.size, lowFirst = false))
    ext.io_sel := port.sel
    ext.io_in := port.in.asUInt
    port.direct := ext.io_out.asTypeOf(port.direct)

    val wrapper = Module(if (c.lowFirst) new SvPriorityMux(chiselTypeOf(port.in.head), c.size)
      else new SvPriorityMux(chiselTypeOf(port.in.head), c.size, lowFirst = false))
    wrapper.io.sel := port.sel
    wrapper.io.in := port.in
    port.explicit := wrapper.io.out

    val data = port.in.toSeq
    val selectors = port.sel.asBools
    val pairInputs = selectors.zip(data)
    val iterableInputs: Iterable[(Bool, Data)] = pairInputs
    port.pairs := (if (c.lowFirst) SvPriorityMux(pairInputs) else SvPriorityMux(pairInputs, lowFirst = false))
    port.iterablePairs := (if (c.lowFirst) SvPriorityMux(iterableInputs) else SvPriorityMux(iterableInputs, lowFirst = false))
    port.seqSel := (if (c.lowFirst) SvPriorityMux(selectors, data) else SvPriorityMux(selectors, data, lowFirst = false))
    port.bitsSel := (if (c.lowFirst) SvPriorityMux(port.sel, data) else SvPriorityMux(port.sel, data, lowFirst = false))
    port.sintSel := (if (c.lowFirst) SvPriorityMux(port.sel.asSInt, data) else SvPriorityMux(port.sel.asSInt, data, lowFirst = false))
    port.boolSingle := SvPriorityMux(port.sel(0), Seq(port.in.head), c.lowFirst)
    val prioritySelectors = if (c.lowFirst) selectors else selectors.reverse
    val priorityData = if (c.lowFirst) data else data.reverse
    val priorityBits = if (c.lowFirst) port.sel else Reverse(port.sel)
    port.nativeSeq := PriorityMux(prioritySelectors, priorityData)
    port.nativePairs := PriorityMux(prioritySelectors.zip(priorityData))
    port.nativeSint := PriorityMux(priorityBits.asSInt, priorityData)
    port.nativeBool := PriorityMux(port.sel(0), Seq(port.in.head))
  }
}

private[test] class SvPriorityMuxDefaultsTop extends Module {
  val io = IO(new Bundle {
    val sel = Input(UInt(2.W))
    val in = Input(Vec(2, new WrapperTestPacket(32)))
    val out = Output(Vec(4, UInt(32.W)))
  })
  val low = Module(new SvPriorityMux())
  val high = Module(new SvPriorityMux(lowFirst = false))
  // These annotations also verify that the default gen infers UInt, without casts.
  val lowResult: UInt = low.io.out
  val highResult: UInt = high.io.out
  for (mux <- Seq(low, high)) {
    require(mux.io.in.size == 2 && mux.io.out.getWidth == 32)
    mux.io.sel := io.sel
    mux.io.in.zip(io.in).foreach { case (lane, packet) => lane := packet.asUInt }
  }
  val lowCore = Module(new SvPriorityMuxExt())
  val highCore = Module(new SvPriorityMuxExt(lowFirst = false))
  for (core <- Seq(lowCore, highCore)) {
    require(core.io_sel.getWidth == 2 && core.io_in.getWidth == 64 && core.io_out.getWidth == 32)
    core.io_sel := io.sel
    core.io_in := io.in.asUInt
  }
  io.out(0) := lowResult
  io.out(1) := highResult
  io.out(2) := lowCore.io_out
  io.out(3) := highCore.io_out
}

private[test] class SvPriorityMuxHeterogeneousBundleTop(useNative: Boolean) extends RawModule {
  val sel = IO(Input(UInt(3.W)))
  val a = IO(Input(new WrapperTestPacket(5)))
  val b = IO(Input(new WrapperTestPacket(13)))
  val c = IO(Input(new WrapperTestPacket(17)))
  val out = IO(Output(new WrapperTestPacket(17)))
  out := (if (useNative) PriorityMux(sel, Seq(a, b, c)) else SvPriorityMux(sel, Seq(a, b, c)))
}

private[test] class SvPriorityMuxHeterogeneousVecTop(useNative: Boolean) extends RawModule {
  val sel = IO(Input(UInt(3.W)))
  val a = IO(Input(Vec(3, new WrapperTestPacket(5))))
  val b = IO(Input(Vec(3, new WrapperTestPacket(13))))
  val c = IO(Input(Vec(3, new WrapperTestPacket(17))))
  val out = IO(Output(Vec(3, new WrapperTestPacket(17))))
  out := (if (useNative) PriorityMux(sel, Seq(a, b, c)) else SvPriorityMux(sel, Seq(a, b, c)))
}

class SvPriorityMuxSpec extends SimTestSpec {
  describe("SvPriorityMux") {
    it("defaults Impl and wrapper to two 32-bit UInt lanes with low priority first") {
      simulate(new SvPriorityMuxDefaultsTop) { dut =>
        val all = (BigInt(1) << 32) - 1
        val vectors = Seq(Seq(BigInt(0), all), Seq(all, BigInt(0)),
          Seq(BigInt(1) << 31, BigInt(1)), Seq(BigInt(0x12345678L), BigInt(0x87654321L)))
        for (data <- vectors; mask <- Seq(0, 1, 2, 3, 0)) {
          dut.io.sel.poke(mask.U)
          dut.io.in.zip(data).foreach { case (lane, value) => pokePayload(lane, value) }
          val lowLane = if ((mask & 1) != 0) 0 else 1
          val highLane = if ((mask & 2) != 0) 1 else 0
          for (index <- Seq(0, 2)) dut.io.out(index).expect(data(lowLane).U)
          for (index <- Seq(1, 3)) dut.io.out(index).expect(data(highLane).U)
        }
      }
    }

    it("exhaustively selects one through six lanes in both priority directions") {
      val cases = for (size <- 1 to 6; lowFirst <- Seq(true, false))
        yield SvPriorityMuxCase(17, size, lowFirst = lowFirst)
      simulate(new SvPriorityMuxBank(cases)) { dut =>
        cases.zipWithIndex.foreach { case (c, index) =>
          val port = dut.io(index)
          val order = if (c.lowFirst) 0 until c.size else (0 until c.size).reverse
          for (mask <- 0 until (1 << c.size)) {
            val data = (0 until c.size).map(lane => (BigInt(1) << 16) | BigInt((mask << 4) | lane))
            val expectedLane = order.find(bit => (mask & (1 << bit)) != 0).getOrElse(order.last)
            val clue = s"case=$c mask=$mask expectedLane=$expectedLane"
            port.sel.poke(mask.U)
            port.in.zip(data).foreach { case (lane, value) => pokePayload(lane, value) }
            Seq(port.direct, port.explicit, port.pairs, port.iterablePairs, port.seqSel,
              port.bitsSel, port.sintSel, port.nativeSeq, port.nativePairs, port.nativeSint)
              .foreach(expectPayload(_, data(expectedLane), clue))
            expectPayload(port.boolSingle, data.head, clue)
            expectPayload(port.nativeBool, data.head, clue)
          }
        }
      }
    }

    it("matches PriorityMux and software priority selection in both directions for Bundle and Vec[Bundle]") {
      val cases = Seq(
        SvPriorityMuxCase(1, 2),
        SvPriorityMuxCase(17, 1),
        SvPriorityMuxCase(17, 2),
        SvPriorityMuxCase(33, 3),
        SvPriorityMuxCase(17, 5),
        SvPriorityMuxCase(97, 7),
        SvPriorityMuxCase(257, 17),
        SvPriorityMuxCase(51, 3, "vecBundle"),
        SvPriorityMuxCase(192, 5, "vecBundle")
      ).flatMap(c => Seq(c, c.copy(lowFirst = false)))
      val seed = testSeed(0x9d10L)
      val cycles = testCycles(96)
      val rng = new Random(seed)
      info(s"seed=$seed cycles=$cycles configurations=${cases.size}")

      simulate(new SvPriorityMuxBank(cases)) { dut =>
        for (cycle <- 0 until cycles; (c, caseIndex) <- cases.zipWithIndex) {
          val port = dut.io(caseIndex)
          val mask = if (cycle == 0) BigInt(0)
            else if (cycle <= c.size) BigInt(1) << (cycle - 1)
            else if (cycle == c.size + 1) (BigInt(1) << c.size) - 1
            else BigInt(c.size, rng)
          val allOnes = (BigInt(1) << c.width) - 1
          val data = Seq.tabulate(c.size) { lane =>
            // No-hit data must make first- and last-lane fallback distinguishable.
            if (cycle == 0) allOnes ^ BigInt(lane)
            else if (cycle <= c.size) allOnes ^ BigInt(1) << (lane % c.width)
            else BigInt(c.width, rng)
          }
          if (cycle == 0) assert(data.distinct.size == data.size, s"case=$c no-hit payloads must be distinct")
          val order = if (c.lowFirst) 0 until c.size else (0 until c.size).reverse
          val chosen = order.find(mask.testBit)
          val expected = data(chosen.getOrElse(order.last))
          val clue = s"seed=$seed cycle=$cycle case=$c mask=$mask chosen=$chosen"

          port.sel.poke(mask.U)
          port.in.zip(data).foreach { case (signal, value) => pokePayload(signal, value) }
          Seq(port.direct, port.explicit, port.pairs, port.iterablePairs, port.seqSel,
            port.bitsSel, port.sintSel, port.nativeSeq, port.nativePairs, port.nativeSint)
            .foreach(expectPayload(_, expected, clue))
          expectPayload(port.boolSingle, data.head, s"$clue boolSingle")
          expectPayload(port.nativeBool, data.head, s"$clue nativeBool")
        }
      }
    }

    it("uses the lowest-priority lane on no hit in both directions across input changes") {
      val cases = Seq(SvPriorityMuxCase(17, 3), SvPriorityMuxCase(257, 17),
        SvPriorityMuxCase(51, 5, "vecBundle")).flatMap(c => Seq(c, c.copy(lowFirst = false)))
      simulate(new SvPriorityMuxBank(cases)) { dut =>
        cases.zipWithIndex.foreach { case (c, index) =>
          val allSelect = (BigInt(1) << c.size) - 1
          val lastTwo = (BigInt(1) << (c.size - 2)) | (BigInt(1) << (c.size - 1))
          // Revisit no-hit after multi-hit, then change only data under no-hit.
          val selections = Seq(BigInt(0), allSelect, BigInt(0), lastTwo, BigInt(0), BigInt(0))
          selections.zipWithIndex.foreach { case (mask, step) =>
            val allData = (BigInt(1) << c.width) - 1
            val base = if (step % 2 == 0) allData else BigInt(0)
            val data = (0 until c.size).map(lane => base ^ BigInt(lane))
            assert(data.distinct.size == c.size)
            val order = if (c.lowFirst) 0 until c.size else (0 until c.size).reverse
            val expectedLane = order.find(mask.testBit).getOrElse(order.last)
            val port = dut.io(index)
            val clue = s"case=$c step=$step mask=$mask expectedLane=$expectedLane"
            port.sel.poke(mask.U)
            port.in.zip(data).foreach { case (signal, value) => pokePayload(signal, value) }
            Seq(port.direct, port.explicit, port.pairs, port.iterablePairs, port.seqSel,
              port.bitsSel, port.sintSel, port.nativeSeq, port.nativePairs, port.nativeSint)
              .foreach(expectPayload(_, data(expectedLane), clue))
          }
        }
      }
    }

    it("rejects heterogeneous Bundle and Vec[Bundle] inputs exactly like native PriorityMux") {
      for (useNative <- Seq(false, true); useVec <- Seq(false, true)) {
        val error = intercept[IllegalArgumentException] {
          if (useVec) ChiselStage.emitCHIRRTL(new SvPriorityMuxHeterogeneousVecTop(useNative))
          else ChiselStage.emitCHIRRTL(new SvPriorityMuxHeterogeneousBundleTop(useNative))
        }
        info(s"native=$useNative vec=$useVec rejected: ${error.getMessage}")
      }
    }

    it("inlines typed wrappers and emits one parameterized template for repeated parameters") {
      val rtl = ChiselStage.emitSystemVerilog(new SvPriorityMuxBank(Seq(
        SvPriorityMuxCase(17, 3),
        SvPriorityMuxCase(17, 3),
        SvPriorityMuxCase(51, 3, "vecBundle"),
        SvPriorityMuxCase(17, 3, lowFirst = false)
      )))
      val names = "(?m)^\\s*module\\s+(SvPriorityMux(?:_[0-9]+)?)\\b".r.findAllMatchIn(rtl)
        .map(_.group(1)).toSeq
      assert(names == Seq("SvPriorityMux"), names.mkString(", "))
      assert(rtl.contains("parameter integer DATA_WIDTH = 32"))
      assert(rtl.contains("parameter integer INPUT_SIZE = 2"))
      assert(rtl.contains("parameter integer LOW_FIRST = 1"))
      assert(rtl.contains(".LOW_FIRST(0)") && rtl.contains(".LOW_FIRST(1)"))
      assert(rtl.contains("io_sel"))
      assert(rtl.contains("io_in"))
      assert(rtl.contains("io_out"))
      assert("(?m)^\\s*SvPriorityMux\\s*#\\(".r.findFirstIn(rtl).nonEmpty)
    }

    it("rejects invalid dimensions and empty or mismatched input lists") {
      for (dataWidth <- Seq(0, -1); inputSize <- Seq(1, 2)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule { val bad = Module(new SvPriorityMuxExt(dataWidth, inputSize)) })
        }
      }
      for (inputSize <- Seq(0, -1)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule { val bad = Module(new SvPriorityMuxExt(8, inputSize)) })
        }
      }
      for (kind <- Seq("impl", "wrapper", "pairs", "iterable", "seq", "bits")) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            kind match {
              case "impl" => Module(new SvPriorityMuxExt(inputSize = 0, lowFirst = false))
              case "wrapper" => Module(new SvPriorityMux(inputSize = 0, lowFirst = false))
              case "pairs" => SvPriorityMux(Seq.empty[(Bool, UInt)], lowFirst = false)
              case "iterable" => SvPriorityMux(List.empty[(Bool, UInt)]: Iterable[(Bool, UInt)], lowFirst = false)
              case "seq" => SvPriorityMux(Seq(true.B), Seq(1.U, 2.U), lowFirst = false)
              case "bits" => SvPriorityMux(0.U, Seq.empty[UInt], lowFirst = false)
            }
          })
        }
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val bad = Module(new SvPriorityMux(UInt(0.W), 2))
        })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val bad = Module(new SvPriorityMux(new WrapperTestPacket(17), 0))
        })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val bad = SvPriorityMux(Seq.empty[(Bool, WrapperTestPacket)])
        })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val bad = SvPriorityMux(Seq.empty[Bool], Seq.empty[WrapperTestPacket])
        })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val bad = SvPriorityMux(0.U, Seq.empty[WrapperTestPacket])
        })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val a = IO(Input(new WrapperTestPacket(17)))
          val bad = SvPriorityMux(Seq(true.B), Seq(a, a))
        })
      }
    }
  }
}
