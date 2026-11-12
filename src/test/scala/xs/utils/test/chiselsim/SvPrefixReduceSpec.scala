package xs.utils.test.chiselsim

import chisel3._
import chisel3.experimental.IntParam
import chisel3.util.MixedVec
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import xs.utils.GlobalData
import xs.utils.verilog._

import scala.util.Random

private[test] case class PrefixReduceCase(size: Int, high: Boolean, op: Int)

private[test] class PrefixReducePort(c: PrefixReduceCase) extends Bundle {
  val in = Input(UInt(c.size.W))
  val direct = Output(UInt(c.size.W))
  val wrapped = Output(UInt(c.size.W))
  val packed = Output(UInt(c.size.W))
  val signed = Output(UInt(c.size.W))
}

private[test] class PrefixReduceBank(cases: Seq[PrefixReduceCase]) extends RawModule {
  val io = IO(MixedVec(cases.map(new PrefixReducePort(_))))
  cases.zip(io).foreach { case (c, port) =>
    val defaults = c == PrefixReduceCase(32, high = false, op = 0)
    val core = Module(if (defaults) new SvPrefixReduceImpl()() else new SvPrefixReduceImpl(c.op)(c.size, c.high))
    val wrapper = Module(if (defaults) new SvPrefixReduce() else new SvPrefixReduce(c.size, c.high, c.op))
    core.io.in := port.in
    wrapper.io.in := port.in
    port.direct := core.io.out
    port.wrapped := wrapper.io.out
    port.packed := (if (defaults) SvPrefixReduce(port.in) else SvPrefixReduce(port.in, c.high, c.op))
    port.signed := SvPrefixReduce(port.in.asSInt, c.high, c.op)
  }
}

private[test] class PrefixReduceDependencyTop(prefixFirst: Boolean, op: Int) extends RawModule {
  val in = IO(Input(UInt(5.W)))
  val reduced = IO(Output(UInt(5.W)))
  val encoded = IO(Output(UInt(5.W)))
  if (prefixFirst) {
    reduced := SvPrefixReduce(in, highPrefix = true, op = op)
    encoded := SvPriorityEncoder1H(in)
  } else {
    encoded := SvPriorityEncoder1H(in)
    reduced := SvPrefixReduce(in, highPrefix = true, op = op)
  }
}

class SvPrefixReduceSpec extends SimTestSpec {
  // Independent per-output slice reference, not the implementation's staged prefix network.
  private def expected(value: BigInt, c: PrefixReduceCase): BigInt = {
    (0 until c.size).foldLeft(BigInt(0)) { (result, bit) =>
      val indices = if (c.high) bit until c.size else 0 to bit
      val ones = indices.count(value.testBit)
      val set = c.op match {
        case 0 => ones != 0
        case 1 => ones == indices.size
        case 2 => ones % 2 != 0
      }
      if (set) result.setBit(bit) else result
    }
  }

  private def exercise(sizes: Seq[Int], exhaustive: Boolean): Unit = {
    val cases = for (size <- sizes; high <- Seq(false, true); op <- 0 to 2)
      yield PrefixReduceCase(size, high, op)
    val seed = testSeed(0x9ef1L)
    val cycles = testCycles(128)
    val rng = new Random(seed)
    info(s"seed=$seed cycles=$cycles exhaustive=$exhaustive configurations=${cases.size}")
    simulateRaw(new PrefixReduceBank(cases)) { dut =>
      dut.io.foreach(_.in.poke(0.U))
      cases.zip(dut.io).foreach { case (c, port) =>
        val all = (BigInt(1) << c.size) - 1
        val alternating = (0 until c.size by 2).foldLeft(BigInt(0))(_.setBit(_))
        val values = if (exhaustive) BigInt(0) to all else
          Seq(BigInt(0), all, alternating, all ^ alternating) ++
            (0 until c.size).flatMap(bit => Seq(BigInt(1) << bit, all.clearBit(bit))) ++
            (0 until c.size).flatMap(bit => {
              val low = (BigInt(1) << (bit + 1)) - 1
              Seq(low, all ^ low)
            }) ++ Seq.fill(cycles)(BigInt(c.size, rng)) ++ Seq(all, BigInt(0))
        values.foreach { value =>
          val want = expected(value, c)
          port.in.poke(value.U)
          val clue = s"case=$c input=0x${value.toString(16)}"
          Seq(port.direct, port.wrapped, port.packed, port.signed).foreach(_.expect(want.U, clue))
        }
      }
    }
  }

  describe("SvPrefixReduce") {
    it("exhaustively reduces every inclusive prefix in both directions for all operations at sizes 1 through 8") {
      exercise(1 to 8, exhaustive = true)
    }

    it("covers defaults, non-power-of-two sizes, word boundaries and randomized inputs") {
      exercise(Seq(9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129), exhaustive = false)
    }

    it("specializes each operation while preserving the procedural packed-tree kernel") {
      for ((operator, op) <- Seq("|", "&", "^").zipWithIndex) {
        val template = SvPrefixReduceImpl(op)
        val rtl = new VlogRegistry().sources(template).last.text
        assert(template.moduleName == Seq("SvPrefixReduceOR", "SvPrefixReduceAND", "SvPrefixReduceXOR")(op))
        assert(rtl.contains(s"localparam bit IDENTITY = 1'b${if (op == SvPrefixReduce.And) 1 else 0};"))
        assert(rtl.contains(s"padded_input[2 * bit_index] $operator padded_input[2 * bit_index + 1]"))
        assert(rtl.contains(s"partial_result[level_offset(level + 1) + 2 * bit_index] $operator"))
        assert(rtl.contains(s"reduced_bits[i] = $operator prefix_terms[i];"))
        assert(rtl.contains("function automatic integer level_offset(input integer level)"))
        assert(rtl.contains("level_offset = (1 << level) - 1;"))
        assert(rtl.contains("logic [level_offset(LEVELS) - 1:0] partial_result;"))
        assert(rtl.contains("always_comb begin"))
        assert(rtl.contains("for (integer level = LEVELS - 1; level >= 0; level = level - 1)"))
        assert(rtl.contains("partial_result[level_offset(level + 1) + 2 * bit_index + 1]"))
        assert("begin : calc_level".r.findAllIn(rtl).size == 1)
        assert("begin : calc_prefix".r.findAllIn(rtl).size == 1)
        assert(!rtl.contains("parameter integer OP"))
        assert(!rtl.contains("if (OP"))
        assert(!rtl.contains("gen_or"))
        assert(!rtl.contains("gen_and"))
        assert(!rtl.contains("gen_xor"))
        assert(!rtl.contains("genvar"))
        assert(!rtl.contains("NODE_INDEX"))
        assert(!rtl.contains("prefix [LEVELS + 1]"))
      }
    }

    it("discovers all standalone operations and registers requested specializations once across sizes and directions") {
      val templates = (0 to 2).map(SvPrefixReduceImpl(_))
      val registry = new VlogRegistry
      assert(registry.sources(SvPrefixReduceImpl.modules) == registry.sources(templates))
      assert(registry.sources(SvPrefixReduceImpl()).map(_.moduleName) == Seq("SvPrefixReduceOR"))
      assert(templates.forall(template => template.modules == Seq(template)))
      val discovered = SvModuleMain.discover().filter(_.moduleName.startsWith("SvPrefixReduce"))
      assert(discovered.map(_.moduleName) == Seq("SvPrefixReduceAND", "SvPrefixReduceOR", "SvPrefixReduceXOR"))
      val rediscovered = SvModuleMain.discover().filter(_.moduleName.startsWith("SvPrefixReduce"))
      assert(registry.sources(rediscovered) == registry.sources(discovered))
      val sources = registry.sources(templates ++ (0 to 2).reverse.map(SvPrefixReduceImpl(_)))
      assert(sources.map(_.moduleName) == templates.map(new VlogRegistry().nameOf(_)))
      val cases = for (size <- Seq(3, 5); high <- Seq(false, true); op <- 0 to 2)
        yield PrefixReduceCase(size, high, op)
      val annotations = ChiselStage.elaborate(new PrefixReduceBank(cases)).annotations.collect {
        case a: BlackBoxInlineAnno => a.name
      }.toSeq
      assert(annotations.sorted == sources.map(_.fileName).sorted)
    }

    it("uses only Scala expansion parameters for module names and passes SV parameters per instance") {
      val registry = new VlogRegistry
      val annotations = ChiselStage.elaborate(new RawModule {
        for (op <- 0 to 2; size <- Seq(1, 5, 33); high <- Seq(false, true)) {
          val core = Module(new SvPrefixReduceImpl(op)(inputSize = size, highPrefix = high))
          core.io.in := 0.U
          assert(core.desiredName == registry.nameOf(SvPrefixReduceImpl(op)))
          assert(core.params == Map(
            "INPUT_SIZE" -> IntParam(BigInt(size)),
            "HIGH_PREFIX" -> IntParam(BigInt(if (high) 1 else 0))
          ))
        }
      }).annotations.collect { case a: BlackBoxInlineAnno => a.name }.toSeq
      val expectedNames = (0 to 2).map(op => registry.nameOf(SvPrefixReduceImpl(op)) + ".sv")
      assert(annotations.sorted == expectedNames.sorted)
    }

    it("bypasses singleton Bool factories for all operations and directions") {
      val rtl = ChiselStage.emitSystemVerilog(new RawModule {
        val in = IO(Input(Bool()))
        val out = IO(Output(Vec(6, UInt(1.W))))
        for (high <- Seq(false, true); op <- 0 to 2)
          out((if (high) 3 else 0) + op) := SvPrefixReduce(in, high, op)
      })
      for (name <- Seq("SvPrefixReduce", "SvPrefixReduceOR", "SvPrefixReduceAND", "SvPrefixReduceXOR"))
        assert(!rtl.contains(s"module $name #("))
      simulateRaw(new PrefixReduceBank((for (high <- Seq(false, true); op <- 0 to 2)
        yield PrefixReduceCase(1, high, op)))) { dut =>
        for (value <- Seq(0, 1, 0); port <- dut.io) {
          port.in.poke(value.U)
          Seq(port.direct, port.wrapped, port.packed, port.signed).foreach(_.expect(value.U))
        }
      }
    }

    it("inlines wrappers and registers the prefix dependency once in either order with namespace prefixes") {
      GlobalData.synchronized {
        val saved = GlobalData.prefix
        try for (prefix <- Seq("", "soc_"); prefixFirst <- Seq(false, true); op <- 0 to 2) {
          GlobalData.prefix = prefix
          val registry = new VlogRegistry
          val sources = registry.sources(SvPriorityEncoder1HImpl)
          val dependencyNames = Seq("SvPrefixReduceOR", "SvPriorityEncoder1H").map(prefix + _)
          assert(sources.map(_.moduleName) == dependencyNames)
          assert(sources.last.text.contains(s"${prefix}SvPrefixReduceOR #(.INPUT_SIZE(INPUT_SIZE)"))
          val expectedNames = (dependencyNames :+ (prefix + SvPrefixReduceImpl(op).moduleName)).distinct
          val annotations = ChiselStage.elaborate(new PrefixReduceDependencyTop(prefixFirst, op)).annotations.collect {
            case a: BlackBoxInlineAnno => a.name
          }.toSeq
          assert(annotations.sorted == expectedNames.map(_ + ".sv").sorted)
          val rtl = ChiselStage.emitSystemVerilog(new PrefixReduceDependencyTop(prefixFirst, op))
          val names = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
          assert(names.sorted == (expectedNames :+ "PrefixReduceDependencyTop").sorted)
          assert(rtl.contains("parameter integer INPUT_SIZE = 32"))
          assert(rtl.contains("parameter integer HIGH_PREFIX = 0"))
          assert(!rtl.contains("parameter integer OP"))
          assert(!rtl.contains(".OP("))
        } finally GlobalData.prefix = saved
      }
    }

    it("rejects invalid sizes and operation codes including the singleton factory bypass") {
      for (op <- Seq(-1, 3)) {
        intercept[IllegalArgumentException] { SvPrefixReduceImpl(op) }
      }
      for ((size, op) <- Seq((0, 0), (-1, 0), (5, -1), (5, 3), (1, -1), (1, 3)); ext <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (ext) { val bad = Module(new SvPrefixReduceImpl(op)(size)) }
            else { val bad = Module(new SvPrefixReduce(size, op = op)) }
          })
        }
      }
      for (size <- Seq(1, 5); op <- Seq(-1, 3)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val in = IO(Input(UInt(size.W)))
            val bad = SvPrefixReduce(in, op = op)
          })
        }
      }
    }

    it("rejects empty or unknown widths and unbound inputs") {
      for (unknown <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val in = if (unknown) Wire(UInt()) else 0.U(0.W)
            val bad = SvPrefixReduce(in)
          })
        }
      }
      for (gen <- Seq(UInt(5.W), SInt(5.W), Bool())) {
        intercept[ExpectedHardwareException] {
          ChiselStage.emitCHIRRTL(new RawModule { val bad = SvPrefixReduce(gen) })
        }
      }
    }
  }
}
