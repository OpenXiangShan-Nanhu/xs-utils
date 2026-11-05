package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance}

/** Flat SV boundary. Lane zero occupies the least-significant DATA_WIDTH bits. */
class SvMux1HImpl(dataWidth: Int = 32, inputSize: Int = 2)
    extends VlogExtModule(Map("DATA_WIDTH" -> dataWidth, "INPUT_SIZE" -> inputSize)) {
  require(dataWidth > 0, "SvMux1H DATA_WIDTH must be positive")
  require(inputSize > 0, "SvMux1H INPUT_SIZE must be positive")

  val io = IO(new Bundle {
    val sel = Input(UInt(inputSize.W))
    val in = Input(UInt((dataWidth * inputSize).W))
    val out = Output(UInt(dataWidth.W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvMux1HImpl)
  rtl.register(this, SvMux1HImpl)
}

object SvMux1HImpl extends VlogModule {
  override val moduleName = "SvMux1H"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer DATA_WIDTH = 32,
       |  parameter integer INPUT_SIZE = 2
       |) (
       |  input  wire  [INPUT_SIZE - 1:0]              io_sel,
       |  input  wire  [INPUT_SIZE * DATA_WIDTH - 1:0] io_in,
       |  output wire  [DATA_WIDTH - 1:0]              io_out
       |);
       |  logic [DATA_WIDTH - 1:0] term [INPUT_SIZE];
       |  logic [DATA_WIDTH - 1:0] red [INPUT_SIZE + 1];
       |
       |  // Chisel equivalent: val data = io_in.asTypeOf(Vec(INPUT_SIZE, UInt(DATA_WIDTH.W)))
       |  //   red := VecInit(io_sel.asBools.zip(data).scanLeft(0.U(DATA_WIDTH.W)) { case (r, (s, d)) => r | Mux(s, d, 0.U(DATA_WIDTH.W)) })
       |  // Zero on miss, bitwise OR on multiple hits; masks with io_sel even for INPUT_SIZE == 1.
       |  always_comb begin
       |    red[0] = '0;
       |    for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : gen_lane
       |      term[i] = io_sel[i] ? io_in[i * DATA_WIDTH +: DATA_WIDTH] : '0;
       |      red[i + 1] = red[i] | term[i];
       |    end
       |  end
       |  assign io_out = red[INPUT_SIZE];
       |endmodule
       |""".stripMargin
}

/** Typed, combinational wrapper; only the parameterized SV instance survives RTL emission.
  * Unlike the single-element factory shortcut, this module always masks with io.sel.
  */
class SvMux1H[T <: Data](gen: T = UInt(32.W), inputSize: Int = 2) extends RawModule {
  require(inputSize > 0, "SvMux1H requires at least one input")
  require(gen.isWidthKnown && gen.getWidth > 0, "SvMux1H requires a known, positive data width")

  val io = IO(new Bundle {
    val sel = Input(UInt(inputSize.W))
    val in = Input(Vec(inputSize, gen.cloneType))
    val out = Output(gen.cloneType)
  })

  private val core = Module(new SvMux1HImpl(gen.getWidth, inputSize))
  core.io.sel := io.sel
  core.io.in := io.in.asUInt
  io.out := core.io.out.asTypeOf(gen)
  inlineInstance(this)
}

/** Mux1H-compatible call shapes. Element zero corresponds to select bit zero.
  * Inputs must have statically known widths at the SV boundary. VecInit normalizes
  * differing scalar widths (including signed extension) before packing.
  * Aggregate inputs must have equivalent field types and widths, like Mux1H.
  */
object SvMux1H {
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T])(implicit sourceInfo: SourceInfo): T = {
    require(in.nonEmpty, "SvMux1H requires non-empty inputs")
    require(sel.size == in.size, "SvMux1H selector and input sizes must match")
    // Match Chisel Mux1H's single-element fast path, which ignores the selector.
    if (in.size == 1) in.head
    else {
      val inputs = VecInit(in)
      val mux = Module(new SvMux1H(chiselTypeOf(inputs.head), inputs.size))
      mux.io.sel := VecInit(sel).asUInt
      mux.io.in := inputs
      mux.io.out
    }
  }

  def apply[T <: Data](in: Iterable[(Bool, T)])(implicit sourceInfo: SourceInfo): T = {
    val pairs = in.toSeq
    apply(pairs.map(_._1), pairs.map(_._2))
  }

  def apply[T <: Data](sel: UInt, in: Seq[T])(implicit sourceInfo: SourceInfo): T =
    apply(in.indices.map(sel(_)), in)

  /** Matches (sel & in).orR, including unequal operand widths and one-bit inputs. */
  def apply(sel: UInt, in: UInt)(implicit sourceInfo: SourceInfo): Bool = {
    require(sel.isWidthKnown && in.isWidthKnown, "SvMux1H requires known packed operand widths")
    val size = math.max(sel.getWidth, in.getWidth)
    if (size == 0) false.B
    else {
      val mux = Module(new SvMux1H(Bool(), size))
      mux.io.sel := sel
      // Bool.asUInt can retain the Bool runtime type, whose pad cannot widen.
      // An explicit UInt wire zero-extends both UInt and Bool operands safely.
      val inputs = Wire(UInt(size.W))
      inputs := in
      mux.io.in := VecInit(inputs.asBools)
      mux.io.out
    }
  }
}
