package xs.utils.verilog
import chisel3._
import xs.utils.GlobalData

class ClockBuffer extends ExtModule {
  val io = FlatIO(new Bundle {
    val I = Input(Clock())
    val O = Output(Clock())
  })
  private val modName = s"${GlobalData.prefix}ClockBuffer"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""
       |// VCS coverage exclude_file
       |module $modName (
       |  input  wire I,
       |  output wire O
       |);
       |  assign O = I;
       |endmodule""".stripMargin)
}