package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{MixedVec, Mux1H}
import circt.stage.ChiselStage
import xs.utils.test.{WrapperTestPacket, WrapperTestPayload}
import xs.utils.verilog.{SvMux1H, SvMux1HImpl => SvMux1HExt}

import scala.util.Random

private[test] case class SvMuxCase(width: Int, size: Int, payload: String = "bundle")
private[test] class SvMuxPort(c: SvMuxCase) extends Bundle {
  val sel = Input(UInt(c.size.W))
  val in = Input(Vec(c.size, WrapperTestPayload(c.width, c.payload)))
  val direct = Output(WrapperTestPayload(c.width, c.payload))
  val explicit = Output(WrapperTestPayload(c.width, c.payload))
  val seq = Output(WrapperTestPayload(c.width, c.payload))
  val pairs = Output(WrapperTestPayload(c.width, c.payload))
  val uintSel = Output(WrapperTestPayload(c.width, c.payload))
}

private[test] class SvMuxBank(cases: Seq[SvMuxCase]) extends Module {
  val io = IO(MixedVec(cases.map(new SvMuxPort(_))))
  cases.zip(io).foreach { case (c, port) =>
    // The raw ExtModule has a packed interface; all wrapper paths stay typed.
    val ext = Module(new SvMux1HExt(c.width, c.size))
    ext.io.sel := port.sel
    ext.io.in := port.in.asUInt
    port.direct := ext.io.out.asTypeOf(port.direct)
    val wrapper = Module(new SvMux1H(chiselTypeOf(port.in.head), c.size))
    wrapper.io.sel := port.sel
    wrapper.io.in := port.in
    port.explicit := wrapper.io.out
    port.seq := SvMux1H(port.sel.asBools, port.in.toSeq)
    port.pairs := SvMux1H(port.sel.asBools.zip(port.in.toSeq))
    port.uintSel := SvMux1H(port.sel, port.in.toSeq)
  }
}

private[test] class SvMuxTypedTop extends Module {
  val sel = IO(Input(UInt(3.W)))
  val a = IO(Input(new WrapperTestPacket(5)))
  val b = IO(Input(new WrapperTestPacket(13)))
  val c = IO(Input(new WrapperTestPacket(17)))
  val av = IO(Input(Vec(3, new WrapperTestPacket(5))))
  val bv = IO(Input(Vec(3, new WrapperTestPacket(13))))
  val cv = IO(Input(Vec(3, new WrapperTestPacket(17))))
  val actual = IO(Output(new WrapperTestPacket(17)))
  val reference = IO(Output(new WrapperTestPacket(17)))
  val actualVec = IO(Output(Vec(3, new WrapperTestPacket(17))))
  val referenceVec = IO(Output(Vec(3, new WrapperTestPacket(17))))
  val packedActual = IO(Output(Vec(3, Bool())))
  val packedReference = IO(Output(Vec(3, Bool())))

  // Chisel requires equivalent aggregate types. Widen through typed connects
  // before either mux, preserving per-field signed/unsigned extension.
  private val words = Wire(Vec(3, new WrapperTestPacket(17)))
  private val vectors = Wire(Vec(3, Vec(3, new WrapperTestPacket(17))))
  words.zip(Seq(a, b, c)).foreach { case (sink, source) => sink := source }
  vectors.zip(Seq(av, bv, cv)).foreach { case (sink, source) => sink := source }
  actual := SvMux1H(sel, words.toSeq)
  reference := Mux1H(sel, words.toSeq)
  actualVec := SvMux1H(sel, vectors.toSeq)
  referenceVec := Mux1H(sel, vectors.toSeq)
  // These overloads explicitly require UInt operands; use aggregate fields.
  packedActual(0) := SvMux1H(sel, b.tag)
  packedReference(0) := Mux1H(sel, b.tag)
  packedActual(1) := SvMux1H(sel(0).asUInt, a.flags(0).asUInt)
  packedReference(1) := Mux1H(sel(0).asUInt, a.flags(0).asUInt)
  packedActual(2) := SvMux1H(sel, a.flags(0).asUInt)
  packedReference(2) := Mux1H(sel, a.flags(0).asUInt)
}

class SvMux1HSpec extends SimTestSpec {
  describe("SvMux1H") {
    it("matches masked-OR semantics for Bundle and Vec[Bundle] in every factory form") {
      val cases = Seq(SvMuxCase(1, 1), SvMuxCase(8, 1), SvMuxCase(1, 3),
        SvMuxCase(8, 2), SvMuxCase(17, 3), SvMuxCase(65, 5), SvMuxCase(9, 7),
        SvMuxCase(15, 1, "vecBundle"), SvMuxCase(51, 3, "vecBundle"), SvMuxCase(66, 5, "vecBundle"))
      val rng = new Random(testSeed())
      simulate(new SvMuxBank(cases)) { dut =>
        for (cycle <- 0 until testCycles(128)) {
          cases.zipWithIndex.foreach { case (c, i) =>
            val mask = if (cycle == 0) BigInt(0)
              else if (cycle <= c.size) BigInt(1) << (cycle - 1)
              else if (cycle == c.size + 1) (BigInt(1) << c.size) - 1
              else BigInt(c.size, rng)
            val data = Seq.tabulate(c.size) { lane =>
              if (cycle == 0 || c.width == 1) (BigInt(1) << c.width) - 1
              else if (cycle <= c.size + 1) ((BigInt(1) << c.width) - 1) ^ (BigInt(1) << (lane % c.width))
              else BigInt(c.width, rng)
            }
            val reduced = data.zipWithIndex.foldLeft(BigInt(0)) {
              case (acc, (word, j)) => if (mask.testBit(j)) acc | word else acc
            }
            val factory = if (c.size == 1) data.head else reduced
            val port = dut.io(i)
            val clue = s"cycle=$cycle case=$c mask=$mask"
            port.sel.poke(mask.U)
            port.in.zip(data).foreach { case (signal, value) => pokePayload(signal, value) }
            expectPayload(port.direct, reduced, s"$clue extmodule")
            expectPayload(port.explicit, reduced, s"$clue explicit")
            expectPayload(port.seq, factory, s"$clue seq")
            expectPayload(port.pairs, factory, s"$clue pairs")
            expectPayload(port.uintSel, factory, s"$clue uintSel")
          }
        }
      }
    }

    it("preserves widened unsigned and signed fields in Bundle and Vec[Bundle] payloads") {
      val rng = new Random(testSeed(0x1234L))
      val widths = Seq(5, 13, 17)
      // Independent software widening: extend fields, never the entire packed word.
      def widen(word: BigInt, width: Int): BigInt = {
        val tagWidth = math.min(5, width - 4)
        val deltaWidth = width - tagWidth - 3
        val deltaBits = (word >> 3) & ((BigInt(1) << deltaWidth) - 1)
        val delta = if (deltaBits.testBit(deltaWidth - 1)) deltaBits - (BigInt(1) << deltaWidth) else deltaBits
        val tag = (word >> (deltaWidth + 3)) & ((BigInt(1) << tagWidth) - 1)
        (tag << 12) | ((delta & 511) << 3) | (word & 7)
      }
      def reduce(words: Seq[BigInt], mask: Int): BigInt =
        words.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (word, lane)) =>
          if ((mask & (1 << lane)) != 0) acc | word else acc
        }
      simulate(new SvMuxTypedTop) { dut =>
        for (cycle <- 0 until testCycles(128); mask <- Seq(0, 1, 2, 4, 7)) {
          val words = widths.map(w => if (cycle == 0) (BigInt(1) << w) - 1 else BigInt(w, rng))
          val vectors = widths.map(w => BigInt(3 * w, rng))
          dut.sel.poke(mask.U)
          Seq(dut.a, dut.b, dut.c).zip(words).foreach { case (signal, value) => pokePayload(signal, value) }
          Seq(dut.av, dut.bv, dut.cv).zip(vectors).foreach { case (signal, value) => pokePayload(signal, value) }
          val expected = reduce(words.zip(widths).map { case (word, width) => widen(word, width) }, mask)
          val expectedVec = reduce(vectors.zip(widths).map { case (word, width) =>
            (0 until 3).foldLeft(BigInt(0)) { case (acc, lane) =>
              acc | (widen(word >> (lane * width), width) << (lane * 17))
            }
          }, mask)
          val clue = s"cycle=$cycle mask=$mask"
          expectPayload(dut.actual, expected, s"$clue actual")
          expectPayload(dut.reference, expected, s"$clue reference")
          expectPayload(dut.actualVec, expectedVec, s"$clue actualVec")
          expectPayload(dut.referenceVec, expectedVec, s"$clue referenceVec")
          val packed = Seq(((words(1) >> 8) & mask) != 0, (mask & 1) != 0 && words.head.testBit(0),
            (mask & 1) != 0 && words.head.testBit(0))
          dut.packedActual.zip(dut.packedReference).zip(packed).foreach { case ((actual, reference), value) =>
            actual.expect(value.B, clue)
            reference.expect(value.B, clue)
          }
        }
      }
    }

    it("inlines typed wrappers and emits one shared SV definition for all parameters") {
      val rtl = ChiselStage.emitSystemVerilog(new SvMuxBank(Seq(SvMuxCase(8, 2), SvMuxCase(51, 3, "vecBundle"))))
      val names = "(?m)^\\s*module\\s+(SvMux1H(?:_[0-9]+)?)\\b".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
      assert(names == Seq("SvMux1H"), names.mkString(", "))
      assert(rtl.contains("parameter integer DATA_WIDTH"))
      assert(rtl.contains("parameter integer INPUT_SIZE"))
    }

    it("rejects invalid dimensions and mismatched or empty input lists") {
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule { val bad = Module(new SvMux1HExt(0, 2)) })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule { val bad = Module(new SvMux1HExt(8, 0)) })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val in = IO(Input(Vec(2, new WrapperTestPacket(17))))
          val bad = SvMux1H(Seq(true.B), in.toSeq)
        })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule { val bad = SvMux1H(Seq.empty[(Bool, WrapperTestPacket)]) })
      }
      // Do not silently accept differently laid-out aggregate inputs that native
      // Mux1H also rejects; callers must first connect into a common typed shape.
      for (native <- Seq(false, true); vector <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val a = IO(Input(WrapperTestPayload(if (vector) 15 else 5, if (vector) "vecBundle" else "bundle")))
            val b = IO(Input(WrapperTestPayload(if (vector) 51 else 17, if (vector) "vecBundle" else "bundle")))
            val bad = if (native) Mux1H(Seq(true.B, false.B), Seq(a, b))
              else SvMux1H(Seq(true.B, false.B), Seq(a, b))
          })
        }
      }
    }
  }
}
