package xs.utils.verilog

import chisel3._

class ClockManagerWrapper extends VlogExtModule {
  val io = FlatIO(new Bundle {
    val cfg = Input(Vec(8, UInt(32.W)))
    val in_clock = Input(Clock())
    val cpu_clock = Output(Clock())
    val lock = Output(Bool())
  })
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(ClockManagerWrapper)
  rtl.register(this, ClockManagerWrapper)
}

object ClockManagerWrapper extends VlogModule {
  override val moduleName = "ClockManagerWrapper"
  override def render(ctx: VlogContext): String =
    s"""
       |module ${ctx.selfName} (
       |  input  wire [31:0] cfg_0,
       |  input  wire [31:0] cfg_1,
       |  input  wire [31:0] cfg_2,
       |  input  wire [31:0] cfg_3,
       |  input  wire [31:0] cfg_4,
       |  input  wire [31:0] cfg_5,
       |  input  wire [31:0] cfg_6,
       |  input  wire [31:0] cfg_7,
       |  input  wire in_clock,
       |  output wire cpu_clock,
       |  output wire lock
       |);
       |  assign cpu_clock = in_clock;
       |  assign lock = 1'b1;
       |endmodule
       |""".stripMargin
}
