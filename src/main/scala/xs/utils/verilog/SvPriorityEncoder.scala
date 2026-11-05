package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}
import chisel3.util.log2Ceil

/** Combinational priority encoder; no hit returns zero. */
class SvPriorityEncoderImpl(inputSize: Int = 2, lowFirst: Boolean = true)
    extends VlogExtModule(Map("INPUT_SIZE" -> inputSize, "LOW_FIRST" -> (if (lowFirst) 1 else 0))) {
  require(inputSize > 0, "SvPriorityEncoder INPUT_SIZE must be positive")

  val io_in = IO(Input(UInt(inputSize.W)))
  val io_out = IO(Output(UInt(math.max(1, log2Ceil(inputSize)).W)))

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvPriorityEncoderImpl)
  rtl.register(this, SvPriorityEncoderImpl)
}

object SvPriorityEncoderImpl extends VlogModule {
  override val moduleName = "SvPriorityEncoder"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer INPUT_SIZE = 2,
       |  parameter integer LOW_FIRST = 1
       |) (
       |  input  wire [INPUT_SIZE - 1:0] io_in,
       |  output wire [((INPUT_SIZE > 1) ? $$clog2(INPUT_SIZE) : 1) - 1:0] io_out
       |);
       |  localparam integer INDEX_WIDTH = (INPUT_SIZE > 1) ? $$clog2(INPUT_SIZE) : 1;
       |  logic [INDEX_WIDTH - 1:0] result [INPUT_SIZE + 1];
       |
       |  // No hit (including a deasserted singleton input) returns zero.
       |  generate
       |    if (LOW_FIRST != 0) begin : gen_low_first
       |      // Compute result[i + 1] first, then let the lower index win.
       |      always_comb begin
       |        result[INPUT_SIZE] = '0;
       |        for (integer i = INPUT_SIZE - 1; i >= 0; i = i - 1) begin : gen_priority
       |          result[i] = io_in[i] ? INDEX_WIDTH'(i) : result[i + 1];
       |        end
       |      end
       |      assign io_out = result[0];
       |    end else begin : gen_high_first
       |      // Scan low to high and let later hits replace earlier ones.
       |      always_comb begin
       |        result[0] = '0;
       |        for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : gen_priority
       |          result[i + 1] = io_in[i] ? INDEX_WIDTH'(i) : result[i];
       |        end
       |      end
       |      assign io_out = result[INPUT_SIZE];
       |    end
       |  endgenerate
       |endmodule
       |""".stripMargin
}

/** Inline wrapper; the output index is max(1, log2Ceil(inputSize)) bits wide. */
class SvPriorityEncoder(inputSize: Int = 2, lowFirst: Boolean = true) extends RawModule {
  require(inputSize > 0, "SvPriorityEncoder requires a positive input size")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(math.max(1, log2Ceil(inputSize)).W))
  })

  private val core = Module(new SvPriorityEncoderImpl(inputSize, lowFirst))
  core.io_in := io.in
  io.out := core.io_out
  inlineInstance(this)
}

/** PriorityEncoder-compatible factories for Bits and Seq[Bool] (including Vec[Bool]).
  * Element zero is bit zero.
  * Inputs must be nonempty and have known width. Low bits win by default; no hit returns zero.
  */
object SvPriorityEncoder {
  def apply(in: Bits)(implicit sourceInfo: SourceInfo): UInt = apply(in, lowFirst = true)

  def apply(in: Bits, lowFirst: Boolean)(implicit sourceInfo: SourceInfo): UInt = {
    require(in.isWidthKnown && in.getWidth > 0, "SvPriorityEncoder requires a known, positive input width")
    requireIsHardware(in, "SvPriorityEncoder input")
    if (in.getWidth == 1) 0.U(1.W)
    else {
      val encoder = Module(new SvPriorityEncoder(in.getWidth, lowFirst))
      encoder.io.in := in.asUInt
      encoder.io.out
    }
  }

  def apply(in: Seq[Bool])(implicit sourceInfo: SourceInfo): UInt = apply(in, lowFirst = true)

  def apply(in: Seq[Bool], lowFirst: Boolean)(implicit sourceInfo: SourceInfo): UInt = {
    require(in.nonEmpty, "SvPriorityEncoder requires non-empty inputs")
    apply(VecInit(in).asUInt, lowFirst)
  }
}
