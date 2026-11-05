package xs.utils.verilog
import chisel3._

class ClockMux extends VlogExtModule {
  val io = FlatIO(new Bundle {
    val S = Input(Bool())
    val I0 = Input(Clock())
    val I1 = Input(Clock())
    val O = Output(Clock())
  })
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(ClockMux)
  rtl.register(this, ClockMux)
}

object ClockMux extends VlogModule {
  override val moduleName = "ClockMux"
  override def render(ctx: VlogContext): String =
    s"""
       |// VCS coverage exclude_file
       |module ${ctx.selfName} (
       |  input  wire S,
       |  input  wire I0,
       |  input  wire I1,
       |  output wire O
       |);
       |  assign O = S ? I1 : I0;
       |endmodule""".stripMargin
}
