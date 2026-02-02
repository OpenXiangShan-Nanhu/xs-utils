package xs.utils.verilog

import chisel3._
import chisel3.util._
import xs.utils.GlobalData

class PriorityEncoderOneHotLow(width:Int) extends ExtModule {
  val io = FlatIO(new Bundle {
    val i_data = Input(UInt(width.W))
    val o_code = Output(UInt(width.W))
  })
  private val modName = s"${GlobalData.prefix}PriorityEncoderOneHotLow$width"
  override val desiredName = modName
  private val bodyStr = Seq.tabulate(width)(i => {
    val dat = BigInt(1) << i
    s"if(i_data[$i]) o_code = ${BigIntToOneHotString(dat, width)};"
  }) :+ s"o_code = ${BigIntToOneHotString(0, width)};"
  setInline(s"$modName.sv",
    s"""// VCS coverage exclude_file
       |module $modName (
       |  input  logic [${width - 1}:0] i_data,
       |  output logic [${width - 1}:0] o_code
       |);
       |  always_comb begin
       |    priority ${bodyStr.mkString("\n" + " " * 4 + "else ")}
       |  end
       |endmodule""".stripMargin)
}

object PriorityEncoderOneHotLow {
  def apply(in:UInt):UInt = {
    val enc = Module(new PriorityEncoderOneHotLow(in.getWidth))
    enc.io.i_data := in
    enc.io.o_code
  }

  def apply(ins: Seq[Bool]):Seq[Bool] = {
    val enc = Module(new PriorityEncoderOneHotLow(ins.size))
    enc.io.i_data := Cat(ins.reverse)
    enc.io.o_code.asBools
  }

  def apply(inv:Vec[Bool]):Vec[Bool] = {
    val enc = Module(new PriorityEncoderOneHotLow(inv.size))
    enc.io.i_data := inv.asUInt
    VecInit(enc.io.o_code.asBools)
  }
}
