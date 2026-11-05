package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance}
import chisel3.util.log2Ceil
import xs.utils.verilog

/** Flat, combinational population count; INPUT_SIZE is the only SV parameter. */
class SvPopcountImpl(inputSize: Int = 2) extends VlogExtModule(Map("INPUT_SIZE" -> inputSize)) {
  require(inputSize > 0, "SvPopcount INPUT_SIZE must be positive")

  val io_in = IO(Input(UInt(inputSize.W)))
  val io_out = IO(Output(UInt(log2Ceil(inputSize + 1).W)))

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvPopcountImpl)
  rtl.register(this, SvPopcountImpl)
}

object SvPopcountImpl extends VlogModule {
  override val moduleName: String = "SvPopcount"
  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer INPUT_SIZE = 2
       |) (
       |  input  wire [INPUT_SIZE - 1:0] io_in,
       |  output wire [$$clog2(INPUT_SIZE + 1) - 1:0] io_out
       |);
       |  localparam integer COUNT_WIDTH = $$clog2(INPUT_SIZE + 1);
       |  localparam integer LEAF_COUNT = 1 << $$clog2(INPUT_SIZE);
       |
       |  logic [COUNT_WIDTH - 1:0] count_tree [1:2 * LEAF_COUNT - 1];
       |
       |  // Chisel Equivalent: PopCount(io_in.asBools).
       |  // Zero-pad to a power of two, then reduceTree(_ +& _). Every
       |  // intermediate fits COUNT_WIDTH; no procedural accumulator chain.
       |  always_comb begin
       |    for (integer i = 0; i < LEAF_COUNT; i = i + 1) begin : gen_leaf
       |      if (i < INPUT_SIZE) begin : gen_live
       |        count_tree[LEAF_COUNT + i] = {{(COUNT_WIDTH - 1){1'b0}}, io_in[i]};
       |      end else begin : gen_padding
       |        count_tree[LEAF_COUNT + i] = '0;
       |      end
       |    end
       |    // Children have larger indices, so descending order visits every
       |    // parent only after both of its children have been assigned.
       |    for (integer i = LEAF_COUNT - 1; i >= 1; i = i - 1) begin : gen_sum
       |      count_tree[i] = count_tree[2 * i] + count_tree[2 * i + 1];
       |    end
       |  end
       |  assign io_out = count_tree[1];
       |endmodule
       |""".stripMargin
}

/** Inline combinational wrapper; only the parameterized SV core remains in RTL. */
class SvPopcount(inputSize: Int = 2) extends RawModule {
  require(inputSize > 0, "SvPopcount requires a positive input size")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(log2Ceil(inputSize + 1).W))
  })

  private val core = Module(new SvPopcountImpl(inputSize))
  core.io_in := io.in
  io.out := core.io_out
  inlineInstance(this)
}

/** PopCount-compatible factories for Seq/Vec/Iterable[Bool] and UInt/SInt/Bool.
 * Like PopCount, empty inputs return zero and a single bit returns that bit.
 * Nonempty packed inputs must have a statically known width at the SV boundary.
 */
object SvPopcount {
  def apply(in: Iterable[Bool])(implicit sourceInfo: SourceInfo): UInt = {
    val bits = in.toSeq
    bits.size match {
      case 0 => 0.U
      case 1 => bits.head.asUInt
      case _ =>
        val popcount = Module(new SvPopcount(bits.size))
        popcount.io.in := VecInit(bits).asUInt
        popcount.io.out
    }
  }

  def apply(in: Bits)(implicit sourceInfo: SourceInfo): UInt = {
    require(in.isWidthKnown, "SvPopcount requires a known input width")
    apply((0 until in.getWidth).map(in(_)))
  }
}