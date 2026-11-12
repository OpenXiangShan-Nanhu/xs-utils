package vlogscan.consumer

import xs.utils.verilog.{SvPrefixReduceImpl, VlogContext, VlogModule, VlogModuleProvider}

// Tests load a fresh copy from a consumer artifact, never constructing Chisel hardware.
object Probe {
  var initialized: Int = 0
  var defaultCalls: Int = 0
  var renderCalls: Int = 0
  var failInitialization: Boolean = false
  var failDefaults: Boolean = false
  var conflict: Boolean = false
}

trait ConsumerFamily extends VlogModuleProvider

object DefaultGenerator extends ConsumerFamily {
  require(!Probe.failInitialization, "consumer initialization failed")
  Probe.initialized += 1

  override def modules: Seq[VlogModule] = {
    require(!Probe.failDefaults, "consumer defaults failed")
    Probe.defaultCalls += 1
    Seq(apply())
  }

  def apply(mode: Int = 7): VlogModule = VlogModule(s"ScanConsumer$mode") { ctx =>
    Probe.renderCalls += 1
    s"""module ${ctx.selfName} #(parameter integer WIDTH = 11) (
       |  input wire [WIDTH-1:0] io_in, output wire [WIDTH-1:0] io_out
       |);
       |  ${ctx.ref(SvPrefixReduceImpl(2))} #(.INPUT_SIZE(WIDTH)) u_prefix (
       |    .io_in(io_in), .io_out(io_out)
       |  );
       |endmodule
       |""".stripMargin
  }
}

object PlainGenerator extends VlogModule {
  Probe.initialized += 1
  override def moduleName: String = if (Probe.conflict) "ScanConsumer7" else "ScanPlain"
  override def render(ctx: VlogContext): String = s"module ${ctx.selfName}; endmodule\n"
}

object Container {
  object NestedGenerator extends VlogModule {
    Probe.initialized += 1
    override val moduleName: String = "ScanNested"
    override def render(ctx: VlogContext): String = s"module ${ctx.selfName}; endmodule\n"
  }
}

// Neither an unrelated singleton nor a class/member object may be instantiated by scanning.
object Unrelated {
  throw new AssertionError("unrelated singleton must not initialize")
}

class NeedsEnclosingInstance extends VlogModuleProvider {
  throw new AssertionError("ordinary provider class must not be instantiated")
  override def modules: Seq[VlogModule] = Seq.empty
  object MemberGenerator extends VlogModule {
    override val moduleName: String = "ScanMember"
    override def render(ctx: VlogContext): String = s"module ${ctx.selfName}; endmodule\n"
  }
}

class OptionalBase
object OptionalUnrelated extends OptionalBase {
  throw new AssertionError("optional unrelated singleton must not initialize")
}
