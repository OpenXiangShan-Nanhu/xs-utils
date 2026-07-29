package xs.utils.verilog
import chisel3._
import xs.utils.GlobalData

class ClockMux extends ExtModule {
  val io = FlatIO(new Bundle {
    val S = Input(Bool())
    val I0 = Input(Clock())
    val I1 = Input(Clock())
    val O = Output(Clock())
  })
  private val modName = s"${GlobalData.prefix}ClockMux"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""
       |// VCS coverage exclude_file
       |module $modName (
       |  input  wire S,
       |  input  wire I0,
       |  input  wire I1,
       |  output wire O
       |);
       |  assign O = S ? I1 : I0;
       |endmodule""".stripMargin)
}
