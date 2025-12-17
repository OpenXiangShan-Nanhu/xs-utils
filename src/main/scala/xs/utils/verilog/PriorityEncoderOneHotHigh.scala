package xs.utils.verilog

import chisel3._
import chisel3.util._
import xs.utils.GlobalData

object BigIntToOneHotString {
  def apply(dat:BigInt, width:Int):String = {
    val str = dat.toString(16)
      .reverse
      .padTo((width + 3) / 4, "0")
      .grouped(4)
      .map(_.mkString(""))
      .toSeq.mkString("_")
      .reverse
    s"$width'h$str"
  }
}

class PriorityEncoderOneHotHigh(width:Int) extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val i_data = Input(UInt(width.W))
    val o_code = Output(UInt(width.W))
  })
  private val modName = s"${GlobalData.prefix}PriorityEncoderOneHotHigh$width"
  override val desiredName = modName
  private val bodyStr = Seq.tabulate(width)(i => {
    val idx = width - 1 - i
    val dat = BigInt(1) << idx
    s"if(i_data[$idx]) o_code = ${BigIntToOneHotString(dat, width)};"
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

object PriorityEncoderOneHotHigh {
  def apply(in:UInt):UInt = {
    val enc = Module(new PriorityEncoderOneHotHigh(in.getWidth))
    enc.io.i_data := in
    enc.io.o_code
  }

  def apply(ins: Seq[Bool]):Seq[Bool] = {
    val enc = Module(new PriorityEncoderOneHotHigh(ins.size))
    enc.io.i_data := Cat(ins)
    enc.io.o_code.asBools
  }

  def apply(inv:Vec[Bool]):Vec[Bool] = {
    val enc = Module(new PriorityEncoderOneHotHigh(inv.size))
    enc.io.i_data := inv.asUInt
    VecInit(enc.io.o_code.asBools)
  }
}