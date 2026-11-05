package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance}

/** Flat, combinational priority mux; low lanes have highest priority by default. */
class SvPriorityMuxImpl(dataWidth: Int = 32, inputSize: Int = 2, lowFirst: Boolean = true)
    extends VlogExtModule(Map(
      "DATA_WIDTH" -> dataWidth, "INPUT_SIZE" -> inputSize, "LOW_FIRST" -> (if (lowFirst) 1 else 0))) {
  require(dataWidth > 0, "SvPriorityMux DATA_WIDTH must be positive")
  require(inputSize > 0, "SvPriorityMux INPUT_SIZE must be positive")

  val io = IO(new Bundle {
    val sel = Input(UInt(inputSize.W))
    val in = Input(UInt((dataWidth * inputSize).W))
    val out = Output(UInt(dataWidth.W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvPriorityMuxImpl)
  rtl.register(this, SvPriorityMuxImpl)
}

object SvPriorityMuxImpl extends VlogModule {
  override val moduleName = "SvPriorityMux"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer DATA_WIDTH = 32,
       |  parameter integer INPUT_SIZE = 2,
       |  parameter integer LOW_FIRST = 1
       |) (
       |  input  wire  [INPUT_SIZE - 1:0]              io_sel,
       |  input  wire  [INPUT_SIZE * DATA_WIDTH - 1:0] io_in,
       |  output wire  [DATA_WIDTH - 1:0]              io_out
       |);
       |  logic [DATA_WIDTH - 1:0] result [INPUT_SIZE];
       |
       |  // Chisel equivalent: val lanes = io_in.asTypeOf(Vec(INPUT_SIZE, UInt(DATA_WIDTH.W)))
       |  //   val pairs = io_sel.asBools.zip(lanes); io_out := PriorityMux(if (LOW_FIRST != 0) pairs else pairs.reverse)
       |  // No hit returns the lowest-priority lane; singleton ignores io_sel.
       |  // Seed directly with that lane: testing its selector would mux identical operands.
       |  generate
       |    if (LOW_FIRST != 0) begin : gen_low_first
       |      // Compute result[i + 1] first, then let the lower index win.
       |      always_comb begin
       |        result[INPUT_SIZE - 1] = io_in[(INPUT_SIZE - 1) * DATA_WIDTH +: DATA_WIDTH];
       |        for (integer i = INPUT_SIZE - 2; i >= 0; i = i - 1) begin : gen_priority
       |          result[i] = io_sel[i] ? io_in[i * DATA_WIDTH +: DATA_WIDTH] : result[i + 1];
       |        end
       |      end
       |      assign io_out = result[0];
       |    end else begin : gen_high_first
       |      // Compute result[i - 1] first, then let the higher index win.
       |      always_comb begin
       |        result[0] = io_in[0 +: DATA_WIDTH];
       |        for (integer i = 1; i < INPUT_SIZE; i = i + 1) begin : gen_priority
       |          result[i] = io_sel[i] ? io_in[i * DATA_WIDTH +: DATA_WIDTH] : result[i - 1];
       |        end
       |      end
       |      assign io_out = result[INPUT_SIZE - 1];
       |    end
       |  endgenerate
       |endmodule
       |""".stripMargin
}

/** Typed inline wrapper; only the parameterized SV instance survives RTL emission. */
class SvPriorityMux[T <: Data](gen: T = UInt(32.W), inputSize: Int = 2, lowFirst: Boolean = true) extends RawModule {
  require(inputSize > 0, "SvPriorityMux requires at least one input")
  require(gen.isWidthKnown && gen.getWidth > 0, "SvPriorityMux requires a known, positive data width")

  val io = IO(new Bundle {
    val sel = Input(UInt(inputSize.W))
    val in = Input(Vec(inputSize, gen.cloneType))
    val out = Output(gen.cloneType)
  })

  private val core = Module(new SvPriorityMuxImpl(gen.getWidth, inputSize, lowFirst))
  core.io.sel := io.sel
  core.io.in := io.in.asUInt
  io.out := core.io.out.asTypeOf(gen)
  inlineInstance(this)
}

/** PriorityMux-compatible factories. Low lanes win by default; lowFirst=false reverses priority.
  * No hit returns the lowest-priority lane: the last lane for low-first, lane zero for high-first.
  * VecInit performs the Chisel-compatible type and width unification before packing.
  */
object SvPriorityMux {
  def apply[T <: Data](in: Seq[(Bool, T)])(implicit sourceInfo: SourceInfo): T = apply(in, lowFirst = true)

  def apply[T <: Data](in: Seq[(Bool, T)], lowFirst: Boolean)(implicit sourceInfo: SourceInfo): T =
    applyPairs(in, lowFirst)

  def apply[T <: Data](in: Iterable[(Bool, T)])(implicit sourceInfo: SourceInfo): T = apply(in, lowFirst = true)

  def apply[T <: Data](in: Iterable[(Bool, T)], lowFirst: Boolean)(implicit sourceInfo: SourceInfo): T =
    applyPairs(in.toSeq, lowFirst)

  def apply[T <: Data](sel: Seq[Bool], in: Seq[T])(implicit sourceInfo: SourceInfo): T =
    apply(sel, in, lowFirst = true)

  def apply[T <: Data](sel: Seq[Bool], in: Seq[T], lowFirst: Boolean)(implicit sourceInfo: SourceInfo): T = {
    require(in.nonEmpty, "SvPriorityMux requires non-empty inputs")
    require(sel.size == in.size, "SvPriorityMux selector and input sizes must match")
    buildOrBypass(sel, in, lowFirst)
  }

  def apply[T <: Data](sel: Bits, in: Seq[T])(implicit sourceInfo: SourceInfo): T =
    apply(sel, in, lowFirst = true)

  def apply[T <: Data](sel: Bits, in: Seq[T], lowFirst: Boolean)(implicit sourceInfo: SourceInfo): T = {
    require(in.nonEmpty, "SvPriorityMux requires non-empty inputs")
    // Match Chisel Bits indexing: only the low in.size selector bits are used.
    val selectors = in.indices.map(sel(_))
    buildOrBypass(selectors, in, lowFirst)
  }

  private def applyPairs[T <: Data](in: Seq[(Bool, T)], lowFirst: Boolean)(implicit sourceInfo: SourceInfo): T = {
    require(in.nonEmpty, "SvPriorityMux requires non-empty inputs")
    buildOrBypass(in.map(_._1), in.map(_._2), lowFirst)
  }

  private def buildOrBypass[T <: Data](sel: Seq[Bool], in: Seq[T], lowFirst: Boolean)(implicit sourceInfo: SourceInfo): T = {
    if (in.size == 1) in.head
    else {
      val inputs = VecInit(in)
      val mux = Module(new SvPriorityMux(chiselTypeOf(inputs.head), inputs.size, lowFirst))
      mux.io.sel := VecInit(sel).asUInt
      mux.io.in := inputs
      mux.io.out
    }
  }
}
