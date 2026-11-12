package xs.utils.test.chiselsim

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.experimental.hierarchy.core.{Definition, Instance}
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import org.scalatest.funspec.AnyFunSpec
import xs.utils.GlobalData
import xs.utils.verilog.{VlogContext, VlogExtModule, VlogModule, VlogRegistry}

private[test] object RegistrySources {
  object Leaf extends VlogModule {
    val moduleName = "RegistryLeaf"
    def render(ctx: VlogContext): String =
      s"module ${ctx.selfName}(input wire io_in, output wire io_out); assign io_out = ~io_in; endmodule\n"
  }
  class Branch(val moduleName: String) extends VlogModule {
    def render(ctx: VlogContext): String =
      s"module ${ctx.selfName}(input wire io_in, output wire io_out); ${ctx.ref(Leaf)} u_leaf(.io_in(io_in), .io_out(io_out)); endmodule\n"
  }
  object Left extends Branch("RegistryLeft")
  object Right extends Branch("RegistryRight")
  object Root extends VlogModule {
    val moduleName = "RegistryRoot"
    def render(ctx: VlogContext): String = s"""module ${ctx.selfName}(input wire io_in, output wire io_out);
      |  wire l, r;
      |  ${ctx.ref(Left)} u_left(.io_in(io_in), .io_out(l));
      |  ${ctx.ref(Right)} u_right(.io_in(io_in), .io_out(r));
      |  assign io_out = l | r;
      |endmodule
      |""".stripMargin
  }
}

private[test] class RegistryRootExt extends VlogExtModule {
  val io_in = IO(Input(Bool()))
  val io_out = IO(Output(Bool()))
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(RegistrySources.Root)
  rtl.register(this, RegistrySources.Root)
}

private[test] class RegistryTestTop extends RawModule {
  val in = IO(Input(UInt(2.W)))
  val out = IO(Output(UInt(2.W)))
  val roots = Seq.fill(2)(Module(new RegistryRootExt))
  roots.zipWithIndex.foreach { case (root, i) => root.io_in := in(i) }
  out := VecInit(roots.map(_.io_out)).asUInt
}

private[test] class RegistryLeafExt extends VlogExtModule {
  val io_in = IO(Input(Bool()))
  val io_out = IO(Output(Bool()))
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(RegistrySources.Leaf)
  rtl.register(this, RegistrySources.Leaf)
}

private[test] class RegistryOrderTop(leafFirst: Boolean) extends RawModule {
  val in = IO(Input(Bool()))
  val out = IO(Output(UInt(2.W)))
  val pair = if (leafFirst) {
    val leaf = Module(new RegistryLeafExt)
    (Module(new RegistryRootExt), leaf)
  } else {
    val root = Module(new RegistryRootExt)
    (root, Module(new RegistryLeafExt))
  }
  pair._1.io_in := in
  pair._2.io_in := in
  out := chisel3.util.Cat(pair._1.io_out, pair._2.io_out)
}

private[test] class RegistryDeadOwnerTop extends RawModule {
  val in = IO(Input(Bool()))
  val out = IO(Output(Bool()))
  private val unused = Module(new RegistryRootExt)
  unused.io_in := in
  private val leaf = Module(new RegistryLeafExt)
  leaf.io_in := in
  out := leaf.io_out
}

@instantiable
private[test] class RegistryHierarchy extends RawModule {
  @public val in = IO(Input(Bool()))
  @public val out = IO(Output(Bool()))
  private val root = Module(new RegistryRootExt)
  root.io_in := in
  out := root.io_out
}

private[test] class RegistryHierarchyTop(definitionFirst: Boolean) extends RawModule {
  val in = IO(Input(Bool()))
  val out = IO(Output(UInt(2.W)))
  val pair = if (definitionFirst) {
    val definition = Definition(new RegistryHierarchy)
    (Instance(definition), Module(new RegistryLeafExt))
  } else {
    val leaf = Module(new RegistryLeafExt)
    (Instance(Definition(new RegistryHierarchy)), leaf)
  }
  pair._1.in := in
  pair._2.io_in := in
  out := chisel3.util.Cat(pair._1.out, pair._2.io_out)
}

private[test] class RegistryConflictTop extends RawModule {
  val in = IO(Input(Bool()))
  val out = IO(Output(Bool()))
  val leaf = Module(new RegistryLeafExt)
  leaf.io_in := in
  out := leaf.io_out
  // Use the raw setInline path to verify it shares the dependency registry cache.
  val conflict = Module(new VlogExtModule {
    override val desiredName = "RegistryConflict"
    setInline("RegistryLeaf.sv", "module RegistryLeaf; wire different; endmodule\n")
  })
}

class VlogRegistrySpec extends AnyFunSpec {
  private def withPrefix[T](prefix: String)(body: => T): T = GlobalData.synchronized {
    val previous = GlobalData.prefix
    GlobalData.prefix = prefix
    try body finally GlobalData.prefix = previous
  }

  describe("VlogRegistry") {
    it("collects transitive diamond dependencies once in deterministic dependency-first order") {
      val rtl = withPrefix("soc_") { new VlogRegistry }
      val first = rtl.sources(RegistrySources.Root)
      assert(first.map(_.moduleName) == Seq("soc_RegistryLeaf", "soc_RegistryLeft", "soc_RegistryRight", "soc_RegistryRoot"))
      assert(first == rtl.sources(RegistrySources.Root))
      assert(first.last.text.contains("soc_RegistryLeft u_left"))
      assert(first(1).text.contains("soc_RegistryLeaf u_leaf"))
      assert(withPrefix("chip_") { new VlogRegistry }.sources(RegistrySources.Root).forall(_.moduleName.startsWith("chip_")))
      assert(withPrefix("") { new VlogRegistry }.sources(RegistrySources.Root).head.moduleName == "RegistryLeaf")
      assert(rtl.sources(RegistrySources.Root) == first)
    }

    it("binds render functions lazily and resolves fresh dependency templates in the current namespace") {
      var renders = 0
      def leaf(invert: Boolean): VlogModule = VlogModule(if (invert) "FunctionInvert" else "FunctionWire") { ctx =>
        renders += 1
        val operator = if (invert) "~" else ""
        s"module ${ctx.selfName}(input wire io_in, output wire io_out); assign io_out = ${operator}io_in; endmodule"
      }
      val root = VlogModule("FunctionRoot") { ctx =>
        s"""module ${ctx.selfName}(input wire io_in, output wire [2:0] io_out);
           |  ${ctx.ref(leaf(true))} a(.io_in(io_in), .io_out(io_out[0]));
           |  ${ctx.ref(leaf(true))} b(.io_in(io_in), .io_out(io_out[1]));
           |  ${ctx.ref(leaf(false))} c(.io_in(io_in), .io_out(io_out[2]));
           |endmodule""".stripMargin
      }
      assert(renders == 0)
      for (prefix <- Seq("soc_", "chip_", "")) withPrefix(prefix) {
        val registry = new VlogRegistry
        val sources = registry.sources(root)
        assert(sources.map(_.moduleName) == Seq("FunctionInvert", "FunctionWire", "FunctionRoot").map(prefix + _))
        assert(sources.last.text.contains(s"${prefix}FunctionInvert a("))
        assert(sources.last.text.contains(s"${prefix}FunctionInvert b("))
        assert(sources.last.text.contains(s"${prefix}FunctionWire c("))
        assert(sources.head.text.contains("assign io_out = ~io_in;"))
        assert(sources(1).text.contains("assign io_out = io_in;"))
        assert(registry.sources(root) == sources)
      }
      assert(renders > 0)
    }

    it("validates names and conflicting definitions supplied by render functions") {
      val a = VlogModule("FunctionDuplicate")(ctx => s"module ${ctx.selfName}; endmodule")
      val b = VlogModule("FunctionDuplicate")(ctx => s"module ${ctx.selfName}; wire different; endmodule")
      val error = intercept[IllegalArgumentException] {
        withPrefix("") { new VlogRegistry }.sources(Seq(a, b))
      }
      assert(error.getMessage.contains("Conflicting SV definitions for FunctionDuplicate"))
      intercept[IllegalArgumentException] {
        withPrefix("") { new VlogRegistry }.sources(VlogModule("bad-name")(_ => ""))
      }
    }

    it("detects cycles even when every function-bound dependency is a fresh template") {
      def cycle(which: Int): VlogModule = VlogModule(s"FunctionCycle$which") { ctx =>
        ctx.ref(cycle(1 - which))
      }
      val error = intercept[IllegalArgumentException] {
        withPrefix("p_") { new VlogRegistry }.sources(cycle(0))
      }
      assert(error.getMessage.contains("p_FunctionCycle0 -> p_FunctionCycle1 -> p_FunctionCycle0"))
    }

    it("registers each shared source once without creating dependency ExtModules") {
      for (prefix <- Seq("soc_", "chip_", "")) withPrefix(prefix) {
        val annotations = ChiselStage.elaborate(new RegistryTestTop).annotations.collect {
          case annotation: BlackBoxInlineAnno => annotation
        }.toSeq
        val expectedFiles = Seq("RegistryRoot", "RegistryLeft", "RegistryRight", "RegistryLeaf")
          .map(name => s"$prefix$name.sv").toSet
        assert(annotations.map(_.name).toSet == expectedFiles)
        assert(annotations.size == expectedFiles.size, "skip duplicate setInline, not just duplicate RTL")
        val fir = ChiselStage.emitCHIRRTL(new RegistryTestTop)
        val extmodules = "(?m)^\\s*extmodule\\s+(\\w+)".r.findAllMatchIn(fir).map(_.group(1)).toSeq
        assert(extmodules.size == 2)
        assert(extmodules.forall(_.startsWith(prefix + "RegistryRoot")))
        val rtl = ChiselStage.emitSystemVerilog(new RegistryTestTop)
        val names = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
        for (name <- Seq("RegistryRoot", "RegistryLeft", "RegistryRight", "RegistryLeaf")) {
          assert(names.count(_ == prefix + name) == 1, s"$name: ${names.mkString(", ")}")
        }
      }
    }

    it("deduplicates direct instances and dependencies in either registration order") {
      for (leafFirst <- Seq(true, false); prefix <- Seq("soc_", "")) withPrefix(prefix) {
        val rtl = ChiselStage.emitSystemVerilog(new RegistryOrderTop(leafFirst))
        for (name <- Seq("RegistryRoot", "RegistryLeft", "RegistryRight", "RegistryLeaf")) {
          assert(s"(?m)^\\s*module\\s+$prefix$name\\b".r.findAllIn(rtl).size == 1)
        }
        assert(!rtl.contains("ExtModule attachment"))
      }
    }

    it("retains a shared dependency when its first registration owner is unused") {
      withPrefix("") {
        val rtl = ChiselStage.emitSystemVerilog(new RegistryDeadOwnerTop)
        assert("(?m)^\\s*module\\s+RegistryLeaf\\b".r.findAllIn(rtl).size == 1)
      }
    }

    it("shares registration state with hierarchy Definitions in either order") {
      for (definitionFirst <- Seq(true, false)) withPrefix("") {
        val rtl = ChiselStage.emitSystemVerilog(new RegistryHierarchyTop(definitionFirst))
        for (name <- Seq("RegistryRoot", "RegistryLeft", "RegistryRight", "RegistryLeaf")) {
          assert(s"(?m)^\\s*module\\s+$name\\b".r.findAllIn(rtl).size == 1)
        }
      }
    }

    it("does not leak registration marks into repeated elaborations of the same top") {
      for (_ <- 0 until 3) withPrefix("repeat_") {
        val rtl = ChiselStage.emitSystemVerilog(new RegistryOrderTop(false))
        assert("(?m)^\\s*module\\s+repeat_RegistryLeaf\\b".r.findAllIn(rtl).size == 1)
        assert(rtl.contains("module repeat_RegistryRoot"))
      }
    }

    it("rejects cross-instance source conflicts and clears marks after a failed elaboration") {
      withPrefix("") {
        val error = intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RegistryConflictTop)
        }
        assert(error.getMessage.contains("Conflicting SV definitions for RegistryLeaf.sv"))
        val rtl = ChiselStage.emitSystemVerilog(new RegistryOrderTop(false))
        assert("(?m)^\\s*module\\s+RegistryLeaf\\b".r.findAllIn(rtl).size == 1)
      }
    }

    it("includes and renames the real SvMimoQueue mux and population-count dependencies") {
      for (prefix <- Seq("soc_", "chip_", "")) withPrefix(prefix) {
        val registry = new VlogRegistry
        val sources = registry.sources(xs.utils.verilog.SvMimoQueueImpl)
        val expected = Set("SvMimoQueue", "SvMux1H", "SvPopcount").map(prefix + _)
        assert(sources.size == expected.size)
        assert(sources.map(_.moduleName).toSet == expected)
        assert(sources.last.moduleName == prefix + "SvMimoQueue")
        for (dependency <- Seq("SvMux1H", "SvPopcount")) {
          assert(sources.last.text.contains(s"$prefix$dependency #("))
          assert(sources.exists(_.fileName == s"$prefix$dependency.sv"))
        }
      }
    }

    it("reports dependency cycles with the full path") {
      object A extends VlogModule {
        val moduleName = "A"
        def render(ctx: VlogContext): String = ctx.ref(B)
      }
      object B extends VlogModule {
        val moduleName = "B"
        def render(ctx: VlogContext): String = ctx.ref(A)
      }
      val error = intercept[IllegalArgumentException] { withPrefix("p_") { new VlogRegistry }.sources(A) }
      assert(error.getMessage.contains("p_A -> p_B -> p_A"))
    }

    it("rejects same-name different-content sources and invalid identifiers") {
      val a = new VlogModule {
        val moduleName = "Duplicate"
        def render(ctx: VlogContext): String = s"module ${ctx.selfName}; endmodule"
      }
      val b = new VlogModule {
        val moduleName = "Duplicate"
        def render(ctx: VlogContext): String = s"module ${ctx.selfName}; wire different; endmodule"
      }
      val root = new VlogModule {
        val moduleName = "Root"
        def render(ctx: VlogContext): String = ctx.ref(a) + ctx.ref(b)
      }
      val error = intercept[IllegalArgumentException] { withPrefix("") { new VlogRegistry }.sources(root) }
      assert(error.getMessage.contains("Conflicting SV definitions for Duplicate"))
      intercept[IllegalArgumentException] { withPrefix("bad-prefix-") { new VlogRegistry }.sources(a) }
    }
  }
}
