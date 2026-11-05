package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}
import chisel3.util.log2Ceil

/** Combinational one-hot to binary encoder. */
class SvOHToUIntImpl(inputSize: Int = 2) extends VlogExtModule(Map("INPUT_SIZE" -> inputSize)) {
  require(inputSize > 0, "SvOHToUInt INPUT_SIZE must be positive")

  val io_in = IO(Input(UInt(inputSize.W)))
  val io_out = IO(Output(UInt(math.max(1, log2Ceil(inputSize)).W)))

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvOHToUIntImpl)
  rtl.register(this, SvOHToUIntImpl)
}

object SvOHToUIntImpl extends VlogModule {
  override val moduleName = "SvOHToUInt"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer INPUT_SIZE = 2
       |) (
       |  input  wire [INPUT_SIZE - 1:0] io_in,
       |  output logic [((INPUT_SIZE > 1) ? $$clog2(INPUT_SIZE) : 1) - 1:0] io_out
       |);
       |  localparam integer INDEX_WIDTH = (INPUT_SIZE > 1) ? $$clog2(INPUT_SIZE) : 1;
       |
       |  logic [INDEX_WIDTH - 1:0][INPUT_SIZE - 1:0] selected;
       |
       |  // Each output bit reduces the input lanes whose index has that bit set.
       |  always_comb begin
       |    for (integer b = 0; b < INDEX_WIDTH; b = b + 1) begin : gen_index_bit
       |      for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : gen_lane
       |        selected[b][i] = (((i >> b) & 1) != 0) ? io_in[i] : 1'b0;
       |      end
       |      io_out[b] = |selected[b];
       |    end
       |  end
       |endmodule
       |""".stripMargin
}

/** Inline wrapper; the output is max(1, log2Ceil(inputSize)) bits wide. */
class SvOHToUInt(inputSize: Int = 2) extends RawModule {
  require(inputSize > 0, "SvOHToUInt requires a positive input size")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(math.max(1, log2Ceil(inputSize)).W))
  })

  private val core = Module(new SvOHToUIntImpl(inputSize))
  core.io_in := io.in
  io.out := core.io_out
  inlineInstance(this)
}

/** OHToUInt-compatible factories. Element zero is bit zero; width selects the low bits.
  * Empty and singleton inputs return a one-bit zero without instantiating the SV core.
  * Like OHToUInt, the public contract assumes exactly one high bit. The implementation
  * ORs the indices of asserted bits (and returns zero for no hit).
  */
object SvOHToUInt {
  def apply(in: Seq[Bool])(implicit sourceInfo: SourceInfo): UInt = {
    if (in.isEmpty) 0.U(1.W) else apply(VecInit(in).asUInt, in.size)
  }

  def apply(in: Vec[Bool])(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(in, "SvOHToUInt input")
    apply(in.asUInt, in.size)
  }

  def apply(in: Bits)(implicit sourceInfo: SourceInfo): UInt = {
    require(in.isWidthKnown, "SvOHToUInt requires a known input width or an explicit width")
    apply(in, in.getWidth)
  }

  def apply(in: Bits, width: Int)(implicit sourceInfo: SourceInfo): UInt = {
    require(width >= 0, "SvOHToUInt width must be nonnegative")
    requireIsHardware(in, "SvOHToUInt input")
    // As in OHToUInt/Log2, width < 2 does not read any input bits.
    if (width < 2) 0.U(1.W)
    else {
      require(!in.isWidthKnown || width <= in.getWidth, "SvOHToUInt width exceeds input width")
      val encoder = Module(new SvOHToUInt(width))
      encoder.io.in := in(width - 1, 0)
      encoder.io.out
    }
  }
}
