package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}

/** Decode a strict-side mask into the adjacent one-hot bit. HIGH_MASK selects the high side. */
class SvMaskToOHImpl(inputSize: Int = 8, highMask: Boolean = false)
    extends VlogExtModule(Map("INPUT_SIZE" -> inputSize, "HIGH_MASK" -> (if (highMask) 1 else 0))) {
  require(inputSize > 0, "SvMaskToOH INPUT_SIZE must be positive")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(inputSize.W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvMaskToOHImpl)
  rtl.register(this, SvMaskToOHImpl)
}

object SvMaskToOHImpl extends VlogModule {
  override val moduleName = "SvMaskToOH"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer INPUT_SIZE = 8,
       |  parameter integer HIGH_MASK = 0
       |) (
       |  input  wire  [INPUT_SIZE - 1:0] io_in,
       |  output wire  [INPUT_SIZE - 1:0] io_out
       |);
       |  logic scan [INPUT_SIZE + 1];
       |  logic [INPUT_SIZE - 1:0] edge_oh;
       |
       |  generate
       |    if (HIGH_MASK != 0) begin : gen_high_mask
       |      // Chisel equivalent: val scan = io_in.asBools.scanLeft(false.B)(_ || _)
       |      //   edge_oh := VecInit(io_in.asBools.zip(scan.dropRight(1)).map { case (b, seen) => b && !seen }).asUInt
       |      //   io_out := (edge_oh >> 1).pad(INPUT_SIZE)
       |      always_comb begin
       |        scan[0] = 1'b0;
       |        for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : gen_bit
       |          scan[i + 1] = scan[i] | io_in[i];
       |          edge_oh[i] = io_in[i] & ~scan[i];
       |        end
       |      end
       |      assign io_out = edge_oh >> 1;
       |    end else begin : gen_low_mask
       |      // Chisel equivalent: val scan = io_in.asBools.scanRight(false.B)(_ || _)
       |      //   edge_oh := VecInit(io_in.asBools.zip(scan.drop(1)).map { case (b, seen) => b && !seen }).asUInt
       |      //   io_out := (edge_oh << 1)(INPUT_SIZE - 1, 0)
       |      always_comb begin
       |        scan[INPUT_SIZE] = 1'b0;
       |        for (integer i = INPUT_SIZE - 1; i >= 0; i = i - 1) begin : gen_bit
       |          scan[i] = scan[i + 1] | io_in[i];
       |          edge_oh[i] = io_in[i] & ~scan[i + 1];
       |        end
       |      end
       |      assign io_out = edge_oh << 1;
       |    end
       |  endgenerate
       |endmodule
       |""".stripMargin
}

/** Inline wrapper. A zero mask or a boundary outside the output width produces zero.
  * For INPUT_SIZE=8, 00001111 (low) or 11100000 (high) produces 00010000.
  * The endpoint one-hot that SvOHToMask maps to zero cannot be recovered.
  * Non-contiguous masks use the highest set bit (low) or lowest set bit (high).
  */
class SvMaskToOH(inputSize: Int = 8, highMask: Boolean = false) extends RawModule {
  require(inputSize > 0, "SvMaskToOH requires a positive input size")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(inputSize.W))
  })

  private val core = Module(new SvMaskToOHImpl(inputSize, highMask))
  core.io.in := io.in
  io.out := core.io.out
  inlineInstance(this)
}

object SvMaskToOH {
  /** Infer the mask width from a hardware Bits value; bit zero is the low end. */
  def apply(in: Bits, highMask: Boolean = false)(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(in, "SvMaskToOH input")
    require(in.isWidthKnown && in.getWidth > 0, "SvMaskToOH requires a known, positive input width")
    val decoder = Module(new SvMaskToOH(in.getWidth, highMask))
    decoder.io.in := in.asUInt
    decoder.io.out
  }
}
