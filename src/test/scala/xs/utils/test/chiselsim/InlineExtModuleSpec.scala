package xs.utils.test.chiselsim

import chisel3._
import xs.utils.test.WrapperTestPacket
import chisel3.experimental.inlineInstance
import circt.stage.ChiselStage
import org.scalatest.funspec.AnyFunSpec

private[test] class InlineBoundaryExt extends ExtModule {
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))

  override def desiredName: String = "InlineBoundaryExt"

  setInline(
    "InlineBoundaryExt.sv",
    """module InlineBoundaryExt (
      |  input  logic [7:0] in,
      |  output logic [7:0] out
      |);
      |  assign out = in ^ 8'h5a;
      |endmodule
      |""".stripMargin
  )
}

private[test] class InlineExtWrapper extends Module {
  val io = IO(new Bundle {
    val in = Input(new WrapperTestPacket(8))
    val out = Output(new WrapperTestPacket(8))
  })

  private val boundary = Module(new InlineBoundaryExt)
  boundary.in := io.in.asUInt
  io.out := boundary.out.asTypeOf(io.out)

  inlineInstance(this)
}

private[test] class InlineExtTop extends Module {
  val io = IO(new Bundle {
    val in = Input(new WrapperTestPacket(8))
    val out = Output(new WrapperTestPacket(8))
  })

  private val wrapper = Module(new InlineExtWrapper)
  wrapper.io.in := io.in
  io.out := wrapper.io.out
}

class InlineExtModuleSpec extends AnyFunSpec {
  describe("inlineInstance around an ExtModule") {
    it("inlines the regular wrapper but retains the external module boundary") {
      val verilog = ChiselStage.emitSystemVerilog(new InlineExtTop)

      assert(!"""(?m)^\s*module\s+InlineExtWrapper\b""".r.findFirstIn(verilog).isDefined,
        s"Inline wrapper module remained in generated SystemVerilog:\n$verilog")
      assert(!"""(?m)^\s*InlineExtWrapper\s+\w+\s*\(""".r.findFirstIn(verilog).isDefined,
        s"Inline wrapper instance remained in generated SystemVerilog:\n$verilog")

      assert("""(?m)^\s*module\s+InlineBoundaryExt\b""".r.findFirstIn(verilog).isDefined,
        s"ExtModule definition or its inline SV attachment is missing:\n$verilog")
      assert("""(?m)^\s*InlineBoundaryExt\s+\w+\s*\(""".r.findFirstIn(verilog).isDefined,
        s"ExtModule instance boundary was not retained:\n$verilog")
      assert(verilog.contains("assign out = in ^ 8'h5a;"),
        s"ExtModule inline SV body was not included in generated SystemVerilog:\n$verilog")
    }
  }
}
