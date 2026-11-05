package xs.utils.verilog
import chisel3._

class ClockBuffer extends VlogExtModule {
  val io = FlatIO(new Bundle {
    val I = Input(Clock())
    val O = Output(Clock())
  })
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(ClockBuffer)
  rtl.register(this, ClockBuffer)
}

object ClockBuffer extends VlogModule {
  override val moduleName = "ClockBuffer"
  override def render(ctx: VlogContext): String =
    s"""
       |// VCS coverage exclude_file
       |module ${ctx.selfName} (
       |  input  wire I,
       |  output wire O
       |);
       |  assign O = I;
       |endmodule""".stripMargin
}
