package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{MixedVec, PopCount, log2Ceil}
import circt.stage.ChiselStage
import xs.utils.test.{WrapperTestBit, WrapperTestPayload}
import xs.utils.verilog.{SvPopcount, SvPopcountImpl => SvPopcountExt, VlogContext, VlogModule, VlogRegistry}

import scala.util.Random

private[test] case class SvPopcountCase(size: Int, payload: String = "bitVec") {
  def gen: Data = if (payload == "bitVec") Vec(size, new WrapperTestBit) else WrapperTestPayload(size, payload)
}

private[test] class SvPopcountPort(c: SvPopcountCase) extends Bundle {
  val in = Input(c.gen)
  val direct = Output(UInt(log2Ceil(c.size + 1).W))
  val explicit = Output(UInt(log2Ceil(c.size + 1).W))
  val packed = Output(UInt(log2Ceil(c.size + 1).W))
  val signed = Output(UInt(log2Ceil(c.size + 1).W))
  val seq = Output(UInt(log2Ceil(c.size + 1).W))
  val vec = Output(UInt(log2Ceil(c.size + 1).W))
  val iterable = Output(UInt(log2Ceil(c.size + 1).W))
  val reference = Output(UInt(log2Ceil(c.size + 1).W))
}

private[test] class SvPopcountBank(cases: Seq[SvPopcountCase]) extends Module {
  val io = IO(MixedVec(cases.map(new SvPopcountPort(_))))
  cases.zip(io).foreach { case (c, port) =>
    // Population count consumes bits by definition; the simulation boundary is
    // still Bundle/Vec[Bundle], driven independently field by field.
    val packed = port.in.asUInt
    val ext = Module(new SvPopcountExt(c.size))
    ext.io_in := packed
    port.direct := ext.io_out
    val wrapper = Module(new SvPopcount(c.size))
    wrapper.io.in := packed
    port.explicit := wrapper.io.out
    port.packed := SvPopcount(packed)
    port.signed := SvPopcount(packed.asSInt)
    port.seq := SvPopcount(packed.asBools)
    port.vec := SvPopcount(VecInit(packed.asBools))
    port.iterable := SvPopcount(packed.asBools.toList: Iterable[Bool])
    port.reference := PopCount(packed)
    require(wrapper.io.out.getWidth == log2Ceil(c.size + 1))
  }
}

private[test] class SvPopcountEmptyTop extends Module {
  val in = IO(Input(new WrapperTestBit))
  val out = IO(Output(Vec(5, UInt(1.W))))
  out(0) := SvPopcount(Seq.empty[Bool])
  private val empty = Wire(Vec(0, Bool()))
  out(1) := SvPopcount(empty)
  out(2) := SvPopcount(0.U(0.W))
  out(3) := SvPopcount(in.flag)
  out(4) := SvPopcount(Seq(in.flag))
}

class SvPopcountSpec extends SimTestSpec {
  private def exercise(cases: Seq[SvPopcountCase], exhaustive: Boolean): Unit = {
    val seed = testSeed(0xc01aL)
    val cycles = testCycles(128)
    val rng = new Random(seed)
    info(s"seed=$seed cycles=$cycles exhaustive=$exhaustive configurations=${cases.size}")
    simulate(new SvPopcountBank(cases)) { dut =>
      def check(c: SvPopcountCase, index: Int, value: BigInt): Unit = {
        val port = dut.io(index)
        val expected = value.bitCount
        val clue = s"seed=$seed case=$c value=0x${value.toString(16)}"
        pokePayload(port.in, value)
        Seq(port.direct, port.explicit, port.packed, port.signed, port.seq, port.vec,
          port.iterable, port.reference).foreach(_.expect(expected.U, clue))
      }
      cases.zipWithIndex.foreach { case (c, index) =>
        val all = (BigInt(1) << c.size) - 1
        val values = if (exhaustive) (BigInt(0) until (BigInt(1) << c.size)).toSeq
          else Seq(BigInt(0), all) ++ (0 until c.size).flatMap(bit => Seq(BigInt(1) << bit, all ^ (BigInt(1) << bit))) ++
            Seq.fill(cycles)(BigInt(c.size, rng))
        values.foreach(value => check(c, index, value))
      }
    }
  }

  describe("SvPopcount") {
    it("exhaustively matches PopCount and software counts for one through eight bits") {
      exercise((1 to 8).map(SvPopcountCase(_)), exhaustive = true)
    }

    it("counts Bundle and Vec[Bundle] inputs across tree and machine-word boundaries") {
      val cases = Seq(9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 97).map(SvPopcountCase(_)) ++
        Seq(SvPopcountCase(17, "bundle"), SvPopcountCase(65, "bundle"),
          SvPopcountCase(51, "vecBundle"), SvPopcountCase(66, "vecBundle"))
      exercise(cases, exhaustive = false)
    }

    it("handles empty inputs and Bool factories without emitting an unnecessary SV core") {
      simulate(new SvPopcountEmptyTop) { dut =>
        for (value <- Seq(false, true)) {
          dut.in.flag.poke(value.B)
          dut.out.take(3).foreach(_.expect(0.U))
          dut.out.drop(3).foreach(_.expect((if (value) 1 else 0).U))
        }
      }
      val rtl = ChiselStage.emitSystemVerilog(new SvPopcountEmptyTop)
      assert(!rtl.contains("module SvPopcount #("))
    }

    it("inlines wrappers and registers one parameterized core with transitive dependencies") {
      val rtl = ChiselStage.emitSystemVerilog(new SvPopcountBank(Seq(SvPopcountCase(3), SvPopcountCase(17))))
      val names = "(?m)^\\s*module\\s+(SvPopcount(?:_[0-9]+)?)\\b".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
      assert(names == Seq("SvPopcount"), names.mkString(", "))
      assert(rtl.contains("parameter integer INPUT_SIZE"))
      assert("(?m)^\\s*SvPopcount\\s*#\\(".r.findFirstIn(rtl).nonEmpty)
      object Parent extends VlogModule {
        val moduleName = "PopcountDependencyParent"
        def render(ctx: VlogContext): String = s"""module ${ctx.selfName}(input wire [2:0] io_in, output wire [1:0] io_out);
          |  ${ctx.ref(xs.utils.verilog.SvPopcountImpl)} #(.INPUT_SIZE(3)) u_count(.io_in(io_in), .io_out(io_out));
          |endmodule
          |""".stripMargin
      }
      val registry = new VlogRegistry
      val sources = registry.sources(Parent)
      assert(sources.map(_.moduleName) == Seq(registry.nameOf(xs.utils.verilog.SvPopcountImpl), registry.nameOf(Parent)))
      assert(sources.last.text.contains(s"${registry.nameOf(xs.utils.verilog.SvPopcountImpl)} #(.INPUT_SIZE(3))"))
    }

    it("rejects nonpositive explicit dimensions and unknown packed input widths") {
      for (size <- Seq(0, -1); ext <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (ext) { val bad = Module(new SvPopcountExt(size)) }
            else { val bad = Module(new SvPopcount(size)) }
          })
        }
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val in = IO(Input(UInt()))
          val bad = SvPopcount(in)
        })
      }
    }
  }
}
