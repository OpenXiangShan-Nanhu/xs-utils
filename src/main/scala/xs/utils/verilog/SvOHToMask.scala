package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}

/** One-hot to mask, excluding the asserted position. HIGH_MASK selects the high side. */
class SvOHToMaskImpl(inputSize: Int = 8, highMask: Boolean = false)
    extends VlogExtModule(Map("INPUT_SIZE" -> inputSize, "HIGH_MASK" -> (if (highMask) 1 else 0))) {
  require(inputSize > 0, "SvOHToMask INPUT_SIZE must be positive")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(inputSize.W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvOHToMaskImpl)
  rtl.register(this, SvOHToMaskImpl)
}

object SvOHToMaskImpl extends VlogModule {
  override val moduleName = "SvOHToMask"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer INPUT_SIZE = 8,
       |  parameter integer HIGH_MASK = 0
       |) (
       |  input  wire  [INPUT_SIZE - 1:0] io_in,
       |  output logic [INPUT_SIZE - 1:0] io_out
       |);
       |  logic scan [INPUT_SIZE + 1];
       |
       |  generate
       |    if (HIGH_MASK != 0) begin : gen_high_mask
       |      // Chisel equivalent: io_out := VecInit(io_in.asBools
       |      //   .scanLeft(false.B)(_ || _).dropRight(1)).asUInt
       |      always_comb begin
       |        scan[0] = 1'b0;
       |        for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : gen_bit
       |          scan[i + 1] = scan[i] | io_in[i];
       |          io_out[i] = scan[i];
       |        end
       |      end
       |    end else begin : gen_low_mask
       |      // Chisel equivalent: io_out := VecInit(io_in.asBools
       |      //   .scanRight(false.B)(_ || _).drop(1)).asUInt
       |      always_comb begin
       |        scan[INPUT_SIZE] = 1'b0;
       |        for (integer i = INPUT_SIZE - 1; i >= 0; i = i - 1) begin : gen_bit
       |          scan[i] = scan[i + 1] | io_in[i];
       |          io_out[i] = scan[i + 1];
       |        end
       |      end
       |    end
       |  endgenerate
       |endmodule
       |""".stripMargin
}

/** Inline wrapper. Zero input produces zero; multiple hits OR their individual masks.
  * For INPUT_SIZE=8, 00010000 produces 00001111 (low) or 11100000 (high).
  */
class SvOHToMask(inputSize: Int = 8, highMask: Boolean = false) extends RawModule {
  require(inputSize > 0, "SvOHToMask requires a positive input size")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(inputSize.W))
  })

  private val core = Module(new SvOHToMaskImpl(inputSize, highMask))
  core.io.in := io.in
  io.out := core.io.out
  inlineInstance(this)
}

object SvOHToMask {
  /** Infer the mask width from a hardware Bits value; bit zero is the low end. */
  def apply(in: Bits, highMask: Boolean = false)(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(in, "SvOHToMask input")
    require(in.isWidthKnown && in.getWidth > 0, "SvOHToMask requires a known, positive input width")
    val mask = Module(new SvOHToMask(in.getWidth, highMask))
    mask.io.in := in.asUInt
    mask.io.out
  }
}
