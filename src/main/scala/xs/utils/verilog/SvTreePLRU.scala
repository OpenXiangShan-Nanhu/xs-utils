package xs.utils.verilog

import chisel3._
import chisel3.util._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}

/** One-hot tree-PLRU victim selector. A valid one-hot touch updates the tree on
  * the next rising edge; the victim is combinational from the current state.
  * Reset is asynchronous, active high, and selects way zero. The complete-tree
  * shape for non-power-of-two sizes need not match other PLRU implementations.
  */
class SvTreePLRUImpl(inputSize: Int = 4) extends VlogExtModule(Map("INPUT_SIZE" -> inputSize)) {
  require(inputSize > 1, "SvTreePLRUImpl INPUT_SIZE must be greater than one")

  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))
  val io = IO(new Bundle {
    val touch = Input(Valid(UInt(inputSize.W)))
    val out = Output(UInt(inputSize.W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvTreePLRUImpl)
  rtl.register(this, SvTreePLRUImpl)
}

object SvTreePLRUImpl extends VlogModule {
  override val moduleName = "SvTreePLRU"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer INPUT_SIZE = 4
       |) (
       |  input  wire                     clock,
       |  input  wire                     reset,
       |  input  wire                     io_touch_valid,
       |  input  wire  [INPUT_SIZE - 1:0] io_touch_bits,
       |  output logic [INPUT_SIZE - 1:0] io_out
       |);
       |  // Requires INPUT_SIZE > 1.
       |  // Complete binary tree with INPUT_SIZE leaves and INPUT_SIZE-1 state bits.
       |  // Leaves are numbered by way in left-to-right order, even when INPUT_SIZE
       |  // is not a power of two. At each node, 0 selects left and 1 selects right
       |  // as the older subtree. Touching a way points its ancestors away from it.
       |  localparam integer LEVELS = $$clog2(INPUT_SIZE);
       |  localparam integer SHALLOW_COUNT = 1 << (LEVELS - 1);
       |  localparam integer EXTRA = INPUT_SIZE - SHALLOW_COUNT;
       |  localparam integer DEEP_COUNT = 2 * EXTRA;
       |  logic [INPUT_SIZE - 2:0]     state_q;
       |  logic [2 * INPUT_SIZE - 2:0] active;
       |  logic [2 * INPUT_SIZE - 2:0] touched;
       |
       |  // Chisel-style structure: active(0) := true.B; for each internal node i,
       |  //   active(2*i+1) := active(i) && !state_q(i)
       |  //   active(2*i+2) := active(i) &&  state_q(i)
       |  function automatic logic [2 * INPUT_SIZE - 2:0] active_tree(input logic [INPUT_SIZE - 2:0] state);
       |    active_tree[0] = 1'b1;
       |    for (integer i = 0; i < INPUT_SIZE - 1; i = i + 1) begin
       |      active_tree[2*i + 1] = active_tree[i] && !state[i];
       |      active_tree[2*i + 2] = active_tree[i] &&  state[i];
       |    end
       |  endfunction
       |  always_comb active = active_tree(state_q);
       |
       |  // Chisel equivalent: leaf(i) = if (i < DEEP_COUNT) (1 << LEVELS)-1+i
       |  //   else SHALLOW_COUNT-1+i-EXTRA; io_out := VecInit(leaf.map(active(_))).asUInt
       |  // Bit zero denotes way zero.
       |  always_comb begin
       |    for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin
       |      if (i < DEEP_COUNT) begin
       |        io_out[i] = active[(1 << LEVELS) - 1 + i];
       |      end else begin
       |        io_out[i] = active[SHALLOW_COUNT - 1 + i - EXTRA];
       |      end
       |    end
       |  end
       |
       |  // Chisel-style structure: touched(leaf(i)) := io_touch_bits(i), using leaf above;
       |  //   touched(i) := touched(2*i+1) || touched(2*i+2) for each internal node.
       |  // Assign leaves first, then visit parents in descending order so children are ready.
       |  always_comb begin
       |    for (integer i = INPUT_SIZE - 1; i < 2 * INPUT_SIZE - 1; i = i + 1) begin
       |      if (i >= (1 << LEVELS) - 1) begin
       |        touched[i] = io_touch_bits[i - ((1 << LEVELS) - 1)];
       |      end else begin
       |        touched[i] = io_touch_bits[i - (SHALLOW_COUNT - 1) + EXTRA];
       |      end
       |    end
       |    for (integer i = INPUT_SIZE - 2; i >= 0; i = i - 1) begin
       |      touched[i] = touched[2*i + 1] | touched[2*i + 2];
       |    end
       |  end
       |
       |  // Chisel equivalent (async reset): val state_q = RegInit(0.U((INPUT_SIZE-1).W))
       |  //   when(io_touch_valid) { for (i <- 0 until INPUT_SIZE-1)
       |  //     when(touched(i)) { state_q(i) := touched(2*i+1) } }
       |  always_ff @(posedge clock or posedge reset) begin
       |    if (reset) begin
       |      state_q <= '0;
       |    end else if (io_touch_valid) begin
       |      for (integer i = 0; i < INPUT_SIZE - 1; i = i + 1) begin
       |        if (touched[i]) state_q[i] <= touched[2*i + 1];
       |      end
       |    end
       |  end
       |endmodule
       |""".stripMargin
}

/** Inline Chisel adapter; touch.bits is one-hot when touch.valid is asserted.
  * A zero touch, or touch.valid=false, holds the replacement state.
  * A single way returns constant one without instantiating the SV core.
  */
class SvTreePLRU(inputSize: Int = 4) extends Module {
  require(inputSize > 0, "SvTreePLRU requires a positive input size")
  override def desiredName: String = "SvTreePLRUInline"

  val io = IO(new Bundle {
    val touch = Input(Valid(UInt(inputSize.W)))
    val out = Output(UInt(inputSize.W))
  })

  if (inputSize == 1) {
    io.out := 1.U(1.W)
  } else {
    val core = Module(new SvTreePLRUImpl(inputSize))
    core.clock := clock
    core.reset := reset.asAsyncReset
    core.io.touch := io.touch
    io.out := core.io.out
  }
  inlineInstance(this)
}

object SvTreePLRU {
  /** Infer the number of ways from a Valid one-hot touch; width one returns constant one. */
  def apply(touch: Valid[UInt])(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(touch, "SvTreePLRU touch")
    apply(touch.bits, touch.valid)
  }

  /** Also accept separate mask and enable signals; width one constructs no module. */
  def apply(touch: UInt, touchValid: Bool)(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(touch, "SvTreePLRU touch")
    requireIsHardware(touchValid, "SvTreePLRU touchValid")
    require(touch.isWidthKnown && touch.getWidth > 0,
      "SvTreePLRU requires a known, positive touch width")
    if (touch.getWidth == 1) {
      1.U(1.W)
    } else {
      val plru = Module(new SvTreePLRU(touch.getWidth))
      plru.io.touch.bits := touch
      plru.io.touch.valid := touchValid
      plru.io.out
    }
  }
}
