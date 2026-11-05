package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}

/** Combinational one-hot priority encoder; no hit returns zero. */
class SvPriorityEncoder1HImpl(inputSize: Int = 2, lowFirst: Boolean = true)
    extends VlogExtModule(Map("INPUT_SIZE" -> inputSize, "LOW_FIRST" -> (if (lowFirst) 1 else 0))) {
  require(inputSize > 0, "SvPriorityEncoder1H INPUT_SIZE must be positive")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(inputSize.W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvPriorityEncoder1HImpl)
  rtl.register(this, SvPriorityEncoder1HImpl)
}

object SvPriorityEncoder1HImpl extends VlogModule {
  override val moduleName = "SvPriorityEncoder1H"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer INPUT_SIZE = 2,
       |  parameter integer LOW_FIRST = 1
       |) (
       |  input  wire  [INPUT_SIZE - 1:0] io_in,
       |  output wire  [INPUT_SIZE - 1:0] io_out
       |);
       |  logic [INPUT_SIZE - 1:0] result [INPUT_SIZE + 1];
       |
       |  // Chisel equivalent: val order = if (LOW_FIRST != 0) 0 until INPUT_SIZE else (0 until INPUT_SIZE).reverse
       |  //   io_out := order.reverse.foldLeft(0.U(INPUT_SIZE.W))((r, i) => Mux(io_in(i), (BigInt(1) << i).U(INPUT_SIZE.W), r))
       |  // No hit returns zero; use INPUT_SIZE-bit constants to preserve selections above bit 31.
       |  generate
       |    if (LOW_FIRST != 0) begin : gen_low_first
       |      // Compute result[i + 1] first, then let the lower index win.
       |      always_comb begin
       |        result[INPUT_SIZE] = '0;
       |        for (integer i = INPUT_SIZE - 1; i >= 0; i = i - 1) begin : gen_priority
       |          result[i] = io_in[i] ? (INPUT_SIZE'(1) << i) : result[i + 1];
       |        end
       |      end
       |      assign io_out = result[0];
       |    end else begin : gen_high_first
       |      // Scan low to high and let later hits replace earlier ones.
       |      always_comb begin
       |        result[0] = '0;
       |        for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : gen_priority
       |          result[i + 1] = io_in[i] ? (INPUT_SIZE'(1) << i) : result[i];
       |        end
       |      end
       |      assign io_out = result[INPUT_SIZE];
       |    end
       |  endgenerate
       |endmodule
       |""".stripMargin
}

/** Inline wrapper; only the reusable parameterized SV core survives RTL emission. */
class SvPriorityEncoder1H(inputSize: Int = 2, lowFirst: Boolean = true) extends RawModule {
  require(inputSize > 0, "SvPriorityEncoder1H requires a positive input size")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(inputSize.W))
  })

  private val core = Module(new SvPriorityEncoder1HImpl(inputSize, lowFirst))
  core.io.in := io.in
  io.out := core.io.out
  inlineInstance(this)
}

/** PriorityEncoderOH-compatible factories: Bits returns UInt, Seq[Bool] returns Seq[Bool].
  * Element zero is bit zero. Packed inputs must have known width. Low bits win by default.
  * Like PriorityEncoderOH, empty sequences return an empty sequence and zero-width Bits
  * return a zero-width zero. A nonempty input with no hit returns zero.
  */
object SvPriorityEncoder1H {
  def apply(in: Bits)(implicit sourceInfo: SourceInfo): UInt = apply(in, lowFirst = true)

  def apply(in: Bits, lowFirst: Boolean)(implicit sourceInfo: SourceInfo): UInt = {
    require(in.isWidthKnown, "SvPriorityEncoder1H requires a known input width")
    requireIsHardware(in, "SvPriorityEncoder1H input")
    if (in.getWidth == 0) 0.U(0.W)
    else if (in.getWidth == 1) in.asUInt
    else {
      val encoder = Module(new SvPriorityEncoder1H(in.getWidth, lowFirst))
      encoder.io.in := in.asUInt
      encoder.io.out
    }
  }

  def apply(in: Seq[Bool])(implicit sourceInfo: SourceInfo): Seq[Bool] = apply(in, lowFirst = true)

  def apply(in: Seq[Bool], lowFirst: Boolean)(implicit sourceInfo: SourceInfo): Seq[Bool] = {
    if (in.isEmpty) Seq.empty
    else {
      val encoded = apply(VecInit(in).asUInt, lowFirst)
      Seq.tabulate(in.size)(encoded(_))
    }
  }
}
