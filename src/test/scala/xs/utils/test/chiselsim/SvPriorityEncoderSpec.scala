package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{MixedVec, PriorityEncoder, PriorityEncoderOH, Reverse, log2Ceil}
import circt.stage.ChiselStage
import xs.utils.test.WrapperTestBit
import xs.utils.verilog.{SvPriorityEncoder, SvPriorityEncoder1H, SvPriorityEncoderImpl, SvPriorityEncoder1HImpl}

import scala.util.Random

private[test] case class SvPriorityEncoderCase(size: Int, lowFirst: Boolean)

private[test] class SvPriorityEncoderPort(c: SvPriorityEncoderCase, oneHot: Boolean) extends Bundle {
  private val outputWidth = if (oneHot) c.size else math.max(1, log2Ceil(c.size))
  val in = Input(Vec(c.size, new WrapperTestBit))
  val direct = Output(UInt(outputWidth.W))
  val explicit = Output(UInt(outputWidth.W))
  val packed = Output(UInt(outputWidth.W))
  val signed = Output(UInt(outputWidth.W))
  val seq = Output(UInt(outputWidth.W))
  val vec = Output(UInt(outputWidth.W))
  val reference = Output(UInt(outputWidth.W))
  val boolSingle = Output(UInt(1.W))
}

private[test] class SvPriorityEncoderBank(cases: Seq[SvPriorityEncoderCase], oneHot: Boolean) extends Module {
  val io = IO(MixedVec(cases.map(new SvPriorityEncoderPort(_, oneHot))))
  cases.zip(io).foreach { case (c, port) =>
    val packed = port.in.asUInt
    val bits = packed.asBools
    val vec = VecInit(bits)
    val useDefaults = c.size == 2 && c.lowFirst
    if (oneHot) {
      val ext = Module(if (useDefaults) new SvPriorityEncoder1HImpl() else new SvPriorityEncoder1HImpl(c.size, c.lowFirst))
      ext.io.in := packed
      port.direct := ext.io.out
      val wrapper = Module(if (useDefaults) new SvPriorityEncoder1H() else new SvPriorityEncoder1H(c.size, c.lowFirst))
      wrapper.io.in := packed
      port.explicit := wrapper.io.out
      port.packed := (if (c.lowFirst) SvPriorityEncoder1H(packed) else SvPriorityEncoder1H(packed, lowFirst = false))
      port.signed := SvPriorityEncoder1H(packed.asSInt, c.lowFirst)
      // Explicit types verify source compatibility with PriorityEncoderOH.
      val seqResult: Seq[Bool] = if (c.lowFirst) SvPriorityEncoder1H(bits) else SvPriorityEncoder1H(bits, lowFirst = false)
      val vecResult: Seq[Bool] = SvPriorityEncoder1H(vec, c.lowFirst)
      port.seq := VecInit(seqResult).asUInt
      port.vec := VecInit(vecResult).asUInt
      port.reference := (if (c.lowFirst) PriorityEncoderOH(packed) else Reverse(PriorityEncoderOH(Reverse(packed))))
      port.boolSingle := SvPriorityEncoder1H(packed(0), c.lowFirst)
      require(wrapper.io.out.getWidth == c.size)
    } else {
      val ext = Module(if (useDefaults) new SvPriorityEncoderImpl() else new SvPriorityEncoderImpl(c.size, c.lowFirst))
      ext.io.in := packed
      port.direct := ext.io.out
      val wrapper = Module(if (useDefaults) new SvPriorityEncoder() else new SvPriorityEncoder(c.size, c.lowFirst))
      wrapper.io.in := packed
      port.explicit := wrapper.io.out
      port.packed := (if (c.lowFirst) SvPriorityEncoder(packed) else SvPriorityEncoder(packed, lowFirst = false))
      port.signed := SvPriorityEncoder(packed.asSInt, c.lowFirst)
      port.seq := (if (c.lowFirst) SvPriorityEncoder(bits) else SvPriorityEncoder(bits, lowFirst = false))
      port.vec := SvPriorityEncoder(vec, c.lowFirst)
      val nativeIndex = if (c.lowFirst) PriorityEncoder(packed)
        else (c.size - 1).U - PriorityEncoder(Reverse(packed))
      // Native binary PriorityEncoder leaves no-hit unspecified; our contract is zero.
      port.reference := Mux(packed.orR, nativeIndex, 0.U)
      port.boolSingle := SvPriorityEncoder(packed(0), c.lowFirst)
      require(wrapper.io.out.getWidth == math.max(1, log2Ceil(c.size)))
    }
  }
}

abstract class SvPriorityEncoderSpecBase(oneHot: Boolean) extends SimTestSpec {
  private val moduleName = if (oneHot) "SvPriorityEncoder1H" else "SvPriorityEncoder"

  private def exercise(sizes: Seq[Int], exhaustive: Boolean): Unit = {
    val cases = for (size <- sizes; lowFirst <- Seq(true, false)) yield SvPriorityEncoderCase(size, lowFirst)
    val seed = testSeed(0xec0deL)
    val cycles = testCycles(128)
    val rng = new Random(seed)
    info(s"seed=$seed cycles=$cycles exhaustive=$exhaustive configurations=${cases.size}")
    simulate(new SvPriorityEncoderBank(cases, oneHot)) { dut =>
      cases.zipWithIndex.foreach { case (c, index) =>
        val port = dut.io(index)
        val all = (BigInt(1) << c.size) - 1
        val values = if (exhaustive) (BigInt(0) to all).toSeq
          else Seq(BigInt(0), all, BigInt(0)) ++
            (0 until c.size).flatMap(bit => Seq(BigInt(1) << bit, all ^ (BigInt(1) << bit), BigInt(0))) ++
            (0 until c.size - 1).map(bit => (BigInt(1) << bit) | (BigInt(1) << (c.size - 1))) ++
            Seq.fill(cycles)(BigInt(c.size, rng)) ++ Seq(all, BigInt(0))
        values.foreach { value =>
          val order = if (c.lowFirst) 0 until c.size else (0 until c.size).reverse
          val chosen = order.find(value.testBit)
          val expected = chosen.map(bit => if (oneHot) BigInt(1) << bit else BigInt(bit)).getOrElse(BigInt(0))
          val clue = s"seed=$seed case=$c value=0x${value.toString(16)} chosen=$chosen"
          pokePayload(port.in, value)
          Seq(port.direct, port.explicit, port.packed, port.signed, port.seq, port.vec,
            port.reference).foreach(_.expect(expected.U, clue))
          port.boolSingle.expect((if (oneHot && value.testBit(0)) 1 else 0).U, clue)
        }
      }
    }
  }

  describe(moduleName) {
    it("exhaustively encodes one through eight bits in both priority directions, including defaults") {
      exercise(1 to 8, exhaustive = true)
    }

    it("matches software and native encoders across non-power-of-two and machine-word boundaries") {
      exercise(Seq(9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 129), exhaustive = false)
    }

    it("inlines wrappers and emits one parameterized template across sizes and directions") {
      val rtl = ChiselStage.emitSystemVerilog(new SvPriorityEncoderBank(Seq(
        SvPriorityEncoderCase(2, true), SvPriorityEncoderCase(3, true),
        SvPriorityEncoderCase(3, true), SvPriorityEncoderCase(3, false)
      ), oneHot))
      val names = ("""(?m)^\s*module\s+(""" + moduleName + """(?:_[0-9]+)?)\b""").r
        .findAllMatchIn(rtl).map(_.group(1)).toSeq
      assert(names == Seq(moduleName), names.mkString(", "))
      assert(rtl.contains("parameter integer INPUT_SIZE = 2"))
      assert(rtl.contains("parameter integer LOW_FIRST = 1"))
      assert(("""(?m)^\s*""" + moduleName + """\s*#\(""").r.findFirstIn(rtl).nonEmpty)
      assert(rtl.contains(".LOW_FIRST(0)"))
      assert(rtl.contains(".LOW_FIRST(1)"))
    }

    it("bypasses singleton factories without registering an SV core") {
      val rtl = ChiselStage.emitSystemVerilog(new RawModule {
        val in = IO(Input(Bool()))
        val out = IO(Output(Vec(4, UInt(1.W))))
        for ((lowFirst, index) <- Seq(true, false).zipWithIndex) {
          out(2 * index) := (if (oneHot) SvPriorityEncoder1H(in, lowFirst) else SvPriorityEncoder(in, lowFirst))
          out(2 * index + 1) := (if (oneHot) VecInit(SvPriorityEncoder1H(Seq(in), lowFirst)).asUInt
            else SvPriorityEncoder(Seq(in), lowFirst))
        }
      })
      assert(!rtl.contains(s"module $moduleName #("))
    }

    it("rejects unbound Chisel types even on singleton bypass paths") {
      for (size <- Seq(1, 2); packed <- Seq(true, false)) {
        intercept[ExpectedHardwareException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (packed) {
              val in = UInt(size.W)
              val bad = if (oneHot) SvPriorityEncoder1H(in) else SvPriorityEncoder(in)
            } else if (oneHot) {
              val bad = SvPriorityEncoder1H(Seq.fill(size)(Bool()))
            } else {
              val bad = SvPriorityEncoder(Seq.fill(size)(Bool()))
            }
          })
        }
      }
    }

    it("validates explicit dimensions and factory inputs like the native API") {
      for (size <- Seq(0, -1); ext <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (oneHot) {
              if (ext) { val bad = Module(new SvPriorityEncoder1HImpl(size)) }
              else { val bad = Module(new SvPriorityEncoder1H(size)) }
            } else {
              if (ext) { val bad = Module(new SvPriorityEncoderImpl(size)) }
              else { val bad = Module(new SvPriorityEncoder(size)) }
            }
          })
        }
      }
      val invalidInputs = if (oneHot) Seq("unknownWidth") else Seq("seq", "vec", "zeroWidth", "unknownWidth")
      for (kind <- invalidInputs) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (kind == "seq" || kind == "vec") {
              val in: Seq[Bool] = if (kind == "seq") Seq.empty[Bool] else Wire(Vec(0, Bool()))
              val bad = SvPriorityEncoder(in)
            } else {
              val in = if (kind == "zeroWidth") 0.U(0.W) else IO(Input(UInt()))
              val bad = if (oneHot) SvPriorityEncoder1H(in) else SvPriorityEncoder(in)
            }
          })
        }
      }
    }

    if (oneHot) {
      it("accepts empty sequences and zero-width Bits exactly like PriorityEncoderOH") {
        val rtl = ChiselStage.emitSystemVerilog(new RawModule {
          val out = IO(Output(UInt(1.W)))
          val emptyVec = Wire(Vec(0, Bool()))
          val defaultSeq: Seq[Bool] = SvPriorityEncoder1H(Seq.empty[Bool])
          val defaultVec: Seq[Bool] = SvPriorityEncoder1H(emptyVec)
          val defaultBits: UInt = SvPriorityEncoder1H(0.U(0.W))
          require(defaultSeq.isEmpty && defaultVec.isEmpty && defaultBits.getWidth == 0)
          require(PriorityEncoderOH(Seq.empty[Bool]).isEmpty)
          require(PriorityEncoderOH(emptyVec).isEmpty)
          require(PriorityEncoderOH(0.U(0.W)).getWidth == 0)
          for (lowFirst <- Seq(true, false)) {
            require(SvPriorityEncoder1H(Seq.empty[Bool], lowFirst).isEmpty)
            require(SvPriorityEncoder1H(emptyVec, lowFirst).isEmpty)
            require(SvPriorityEncoder1H(0.U(0.W), lowFirst).getWidth == 0)
          }
          out := defaultBits
        })
        assert(!rtl.contains(s"module $moduleName #("))
      }
    }
  }
}

class SvPriorityEncoderSpec extends SvPriorityEncoderSpecBase(oneHot = false)
class SvPriorityEncoder1HSpec extends SvPriorityEncoderSpecBase(oneHot = true)
