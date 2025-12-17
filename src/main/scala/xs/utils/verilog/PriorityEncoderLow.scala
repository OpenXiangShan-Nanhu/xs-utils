package xs.utils.verilog

import chisel3._
import chisel3.experimental.noPrefix
import chisel3.util._
import xs.utils.GlobalData

class PriorityEncoderLow(width:Int) extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val i_data = Input(UInt(width.W))
    val o_code = Output(UInt(log2Ceil(width).W))
  })
  private val modName = s"${GlobalData.prefix}PriorityEncoderLow$width"
  override val desiredName = modName
  private val bodyStr = Seq.tabulate(width)(i => {
    s"if(i_data[$i]) o_code = ${io.o_code.getWidth}'d$i;"
  }) :+ s"o_code = ${io.o_code.getWidth}'d0;"
  setInline(s"$modName.sv",
    s"""// VCS coverage exclude_file
       |module $modName (
       |  input  logic [${io.i_data.getWidth - 1}:0] i_data,
       |  output logic [${io.o_code.getWidth - 1}:0] o_code
       |);
       |  always_comb begin
       |    priority ${bodyStr.mkString("\n" + " " * 4 + "else ")}
       |  end
       |endmodule""".stripMargin)
}

object PriorityEncoderLow {
  def apply(in:UInt):UInt = {
    val enc = Module(new PriorityEncoderLow(in.getWidth))
    enc.io.i_data := in
    enc.io.o_code
  }

  def apply(ins: Seq[Bool]):Seq[Bool] = {
    val enc = Module(new PriorityEncoderLow(ins.size))
    enc.io.i_data := Cat(ins)
    enc.io.o_code.asBools
  }

  def apply(inv:Vec[Bool]):Vec[Bool] = {
    val enc = Module(new PriorityEncoderLow(inv.size))
    enc.io.i_data := inv.asUInt
    VecInit(enc.io.o_code.asBools)
  }
}
