package xs.utils.verilog

import chisel3._
import xs.utils.GlobalData

class ClockManagerWrapper extends ExtModule {
  val io = FlatIO(new Bundle {
    val cfg = Input(Vec(8, UInt(32.W)))
    val in_clock = Input(Clock())
    val cpu_clock = Output(Clock())
    val lock = Output(Bool())
  })
  private val modName = s"${GlobalData.prefix}ClockManagerWrapper"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""
       |module $modName (
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
       |""".stripMargin)
}
