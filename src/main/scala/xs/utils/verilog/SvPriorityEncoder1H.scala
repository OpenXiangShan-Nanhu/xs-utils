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
       |  output logic [INPUT_SIZE - 1:0] io_out
       |);
       |  wire [INPUT_SIZE - 1:0] prefix;
       |  ${ctx.ref(SvPrefixReduceImpl(0))} #(.INPUT_SIZE(INPUT_SIZE), .HIGH_PREFIX(int'(LOW_FIRST == 0))) u_prefix (
       |    .io_in(io_in), .io_out(prefix)
       |  );
       |
       |  // Chisel equivalent: io_out(i) := io_in(i) && !OR(all higher-priority input bits).
       |  // The inclusive prefix at the adjacent bit excludes the current request.
       |  generate
       |    if (LOW_FIRST != 0) begin : gen_low_first
       |      always_comb begin
       |        io_out[0] = io_in[0];
       |        for (integer i = 1; i < INPUT_SIZE; i = i + 1) begin : gen_priority
       |          io_out[i] = io_in[i] & ~prefix[i - 1];
       |        end
       |      end
       |    end else begin : gen_high_first
       |      always_comb begin
       |        io_out[INPUT_SIZE - 1] = io_in[INPUT_SIZE - 1];
       |        for (integer i = 0; i < INPUT_SIZE - 1; i = i + 1) begin : gen_priority
       |          io_out[i] = io_in[i] & ~prefix[i + 1];
       |        end
       |      end
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
