// Chisel wrapper for the SystemVerilog PLRU cores in this folder:
//   SvPLRUSelectTreeExplicit.sv  -> state -> victim way  (combinational)
//   SvPLRUUpdate.sv              -> (state, touch way) -> next state (combinational)
//
// It follows the xs-utils "Sv*" integration pattern (see SvPriorityEncoder.scala):
//   object  ..Impl extends VlogModule   : parameterized SV template
//   class   ..Impl extends VlogExtModule: flat Chisel ports + SV parameter + registration
//   class   ..     extends RawModule    : typed inline wrapper
//   object  ..                         : apply factories with input checks
//   class   SvPLRU(ways)               : ReplacementPolicy-compatible policy shim
//
// To use it in xs-utils, drop this file at
//   src/main/scala/xs/utils/verilog/SvPLRU.scala
// The package below already matches that location.
package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}
import chisel3.util.log2Ceil

/** Tree-PLRU victim selector (source: sv_plru/SvPLRUSelectTreeExplicit.sv).
  * The state bit layout matches the rocket-chip PseudoLRU tree, so a state read from an
  * existing `ReplacementPolicy` PLRU array can be fed in directly.
  */
class SvPLRUSelectTreeExplicitImpl(ways: Int = 4)
    extends VlogExtModule(Map("WAYS" -> ways)) {
  require(ways > 1 && (ways & (ways - 1)) == 0,
    s"SvPLRUSelectTreeExplicit WAYS must be a power of two greater than one, got $ways")

  val io = IO(new Bundle {
    val select_state = Input(UInt((ways - 1).W))
    val victim_way   = Output(UInt(math.max(1, log2Ceil(ways)).W))//find the max output 
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvPLRUSelectTreeExplicitImpl)
  rtl.register(this, SvPLRUSelectTreeExplicitImpl)
}

object SvPLRUSelectTreeExplicitImpl extends VlogModule {
  override val moduleName = "SvPLRUSelectTreeExplicit"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer WAYS = 4
       |) (
       |  input  wire [WAYS - 2:0]         io_select_state,
       |  output wire [$$clog2(WAYS) - 1:0] io_victim_way
       |);
       |  localparam integer WAY_BITS = (WAYS > 1) ? $$clog2(WAYS) : 1;
       |
       |  function automatic integer plru_node_pos(input integer level, input integer prefix);
       |    integer pos;
       |    integer span;
       |    begin
       |      pos  = WAYS - 2;
       |      span = WAYS;
       |      for (integer j = 0; j < level; j++) begin
       |        if (((prefix >> (level - 1 - j)) & 1) != 0)
       |          pos = pos - 1;
       |        else
       |          pos = pos - span / 2;
       |        span = span / 2;
       |      end
       |      plru_node_pos = pos;
       |    end
       |  endfunction
       |
       |  generate
       |    if (WAYS < 2 || (WAYS & (WAYS - 1)) != 0) begin : gen_invalid_ways
       |      $$error("SvPLRUSelectTreeExplicit: WAYS must be a power of two greater than one");
       |    end else begin : gen_valid_ways
       |      wire [WAY_BITS - 1:0] path [0:WAYS - 2];
       |
       |      // Build bottom-up. height 0 = nodes directly above the leaves,
       |      // height WAY_BITS-1 = root.
       |      for (genvar height = 0; height < WAY_BITS; height++) begin : gen_level
       |        localparam integer LEVEL      = WAY_BITS - 1 - height; // 0 = root
       |        localparam integer SPAN       = 1 << (height + 1);     // subtree size
       |        localparam integer NUM_NODES  = 1 << LEVEL;
       |        localparam integer CHILD_SPAN = SPAN / 2;
       |
       |        for (genvar i = 0; i < NUM_NODES; i++) begin : generate_node
       |          localparam integer POS = plru_node_pos(LEVEL, i);
       |
       |          if (height == 0) begin : generate_leaf_parent
       |            // One decision left: this node's own state bit, zero-extended.
       |            if (WAY_BITS == 1) begin : generate_single_decision
       |              assign path[POS] = io_select_state[POS];
       |            end else begin : generate_pad_path
       |              assign path[POS] = {{(WAY_BITS - 1){1'b0}}, io_select_state[POS]};
       |            end
       |          end else begin : generate_inner_node
       |            localparam integer UPPER      = POS - 1;
       |            localparam integer LOWER      = POS - CHILD_SPAN;
       |            localparam integer CHILD_BITS = WAY_BITS - LEVEL - 1; // >= 1 here
       |            wire [CHILD_BITS - 1:0] child_path;
       |
       |            // Select the subtree with this node's state bit.
       |            assign child_path = io_select_state[POS]
       |                              ? path[UPPER][CHILD_BITS - 1:0]
       |                              : path[LOWER][CHILD_BITS - 1:0];
       |
       |            // Prepend this node's decision to the chosen subtree's decisions.
       |            if (LEVEL == 0) begin : generate_root_path
       |              assign path[POS] = { io_select_state[POS], child_path };
       |            end else begin : generate_subtree_path
       |              assign path[POS] = { {LEVEL{1'b0}}, io_select_state[POS], child_path };
       |            end
       |          end
       |        end
       |      end
       |
       |      assign io_victim_way = path[WAYS - 2];
       |    end
       |  endgenerate
       |endmodule
       |""".stripMargin
}

/** Inline wrapper for the combinational victim selector. */
class SvPLRUSelectTreeExplicit(ways: Int = 4) extends RawModule {
  require(ways > 1 && (ways & (ways - 1)) == 0,
    s"SvPLRUSelectTreeExplicit requires power-of-two ways greater than one, got $ways")

  val io = IO(new Bundle {
    val state     = Input(UInt((ways - 1).W))
    val victimWay = Output(UInt(math.max(1, log2Ceil(ways)).W))
  })

  private val core = Module(new SvPLRUSelectTreeExplicitImpl(ways))
  core.io.select_state := io.state
  io.victimWay := core.io.victim_way
  inlineInstance(this)
}

object SvPLRUSelectTreeExplicit {
  /** Victim way for `state`; the way count is inferred from the state width (ways = state width + 1). */
  def apply(state: Bits)(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(state, "SvPLRUSelectTreeExplicit state")
    require(state.isWidthKnown && state.getWidth > 0,
      "SvPLRUSelectTreeExplicit requires a known, positive state width")
    val ways = state.getWidth + 1
    require((ways & (ways - 1)) == 0,
      s"SvPLRUSelectTreeExplicit state width must be one less than a power of two, got ${state.getWidth}")

    val selector = Module(new SvPLRUSelectTreeExplicit(ways))
    selector.io.state := state.asUInt
    selector.io.victimWay
  }
}

/** Tree-PLRU state update (source: sv_plru/SvPLRUUpdate.sv).
  * A touch moves the tree away from `io_touch_way`; the result is the next state value.
  */
class SvPLRUUpdateImpl(ways: Int = 4)
    extends VlogExtModule(Map("WAYS" -> ways)) {
  require(ways > 1 && (ways & (ways - 1)) == 0,
    s"SvPLRUUpdate WAYS must be a power of two greater than one, got $ways")

  val io = IO(new Bundle {
    val raw_state  = Input(UInt((ways - 1).W))
    val touch_way  = Input(UInt(math.max(1, log2Ceil(ways)).W))
    val next_state = Output(UInt((ways - 1).W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvPLRUUpdateImpl)
  rtl.register(this, SvPLRUUpdateImpl)
}

object SvPLRUUpdateImpl extends VlogModule {
  override val moduleName = "SvPLRUUpdate"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer WAYS = 4
       |) (
       |  input  wire  [WAYS - 2:0]         io_raw_state,
       |  input  wire  [$$clog2(WAYS) - 1:0] io_touch_way,
       |  output logic [WAYS - 2:0]         io_next_state
       |);
       |  localparam integer WAY_BITS = (WAYS > 1) ? $$clog2(WAYS) : 1;
       |
       |  function automatic integer plru_node_pos(input integer level, input integer prefix);
       |    integer pos;
       |    integer span;
       |    begin
       |      pos  = WAYS - 2;
       |      span = WAYS;
       |      for (integer j = 0; j < level; j++) begin
       |        if (((prefix >> (level - 1 - j)) & 1) != 0)
       |          pos = pos - 1;
       |        else
       |          pos = pos - span / 2;
       |        span = span / 2;
       |      end
       |      plru_node_pos = pos;
       |    end
       |  endfunction
       |
       |  generate
       |    if (WAYS < 2 || (WAYS & (WAYS - 1)) != 0) begin : gen_invalid_ways
       |      $$error("SvPLRUUpdate: WAYS must be a power of two greater than one");
       |    end else begin : gen_valid_ways
       |      wire [WAYS - 2:0] next_state;
       |
       |      for (genvar n = 0; n < WAYS - 1; n++) begin : gen_node//一个一个节点扫描
       |        localparam integer LEVEL  = $$clog2(n + 2) - 1;
       |        localparam integer PREFIX = n - ((1 << LEVEL) - 1);//该节点前面的路劲
       |        localparam integer POS    = plru_node_pos(LEVEL, PREFIX);
       |
       |        if (LEVEL == 0) begin : gen_root
       |          assign next_state[POS] = !io_touch_way[WAY_BITS - 1];
       |        end else begin : gen_branch
       |          wire on_path;
       |          assign on_path = (io_touch_way[WAY_BITS - 1 : WAY_BITS - LEVEL] == LEVEL'(PREFIX));
       |          assign next_state[POS] = on_path ? !io_touch_way[WAY_BITS - 1 - LEVEL]
       |                                           : io_raw_state[POS];
       |        end
       |      end
       |
       |      always_comb io_next_state = next_state;
       |    end
       |  endgenerate
       |endmodule
       |""".stripMargin
}

/** Inline wrapper for the combinational state update. */
class SvPLRUUpdate(ways: Int = 4) extends RawModule {
  require(ways > 1 && (ways & (ways - 1)) == 0,
    s"SvPLRUUpdate requires power-of-two ways greater than one, got $ways")

  val io = IO(new Bundle {
    val state     = Input(UInt((ways - 1).W))
    val touchWay  = Input(UInt(math.max(1, log2Ceil(ways)).W))
    val nextState = Output(UInt((ways - 1).W))
  })

  private val core = Module(new SvPLRUUpdateImpl(ways))
  core.io.raw_state := io.state
  core.io.touch_way := io.touchWay
  io.nextState := core.io.next_state
  inlineInstance(this)
}

object SvPLRUUpdate {
  /** Next PLRU state after touching `touchWay`; the way count is inferred from the state width. */
  def apply(state: Bits, touchWay: Bits)(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(state, "SvPLRUUpdate state")
    requireIsHardware(touchWay, "SvPLRUUpdate touchWay")
    require(state.isWidthKnown && state.getWidth > 0,
      "SvPLRUUpdate requires a known, positive state width")
    require(touchWay.isWidthKnown, "SvPLRUUpdate requires a known touch-way width")
    val ways = state.getWidth + 1
    require((ways & (ways - 1)) == 0,
      s"SvPLRUUpdate state width must be one less than a power of two, got ${state.getWidth}")
    val wayWidth = math.max(1, log2Ceil(ways))
    require(touchWay.getWidth == wayWidth,
      s"SvPLRUUpdate touchWay must be $wayWidth bits for $ways ways, got ${touchWay.getWidth}")

    val updater = Module(new SvPLRUUpdate(ways))
    updater.io.state := state.asUInt
    updater.io.touchWay := touchWay.asUInt
    updater.io.nextState
  }
}

/** Policy shim with the same `nBits` / `get_replace_way` / `get_next_state` surface as
  * rocket-chip's `PseudoLRU`, so a `ReplacementPolicy` call site can switch implementations
  * by only changing its construction line. The methods are purely combinational and must be
  * called from inside the module that owns the replacement pipeline; nothing is registered here.
  */
class SvPLRU(ways: Int) {
  require(ways > 1 && (ways & (ways - 1)) == 0,
    s"SvPLRU requires power-of-two ways greater than one, got $ways")

  /** Replacement-state width, equal to rocket-chip `PseudoLRU.nBits`. */
  val nBits: Int = ways - 1

  private val wayWidth = math.max(1, log2Ceil(ways))

  def get_replace_way(state: Bits): UInt = {
    require(state.isWidthKnown, "SvPLRU.get_replace_way requires a known state width")
    require(state.getWidth == nBits,
      s"SvPLRU.get_replace_way expects $nBits state bits for $ways ways, got ${state.getWidth}")
    SvPLRUSelectTreeExplicit(state)
  }

  def get_next_state(state: Bits, touchWay: Bits): UInt = {
    require(state.isWidthKnown, "SvPLRU.get_next_state requires a known state width")
    require(state.getWidth == nBits,
      s"SvPLRU.get_next_state expects $nBits state bits for $ways ways, got ${state.getWidth}")
    require(touchWay.isWidthKnown, "SvPLRU.get_next_state requires a known touch-way width")
    require(touchWay.getWidth == wayWidth,
      s"SvPLRU.get_next_state expects a $wayWidth-bit touch way for $ways ways, got ${touchWay.getWidth}")
    SvPLRUUpdate(state, touchWay)
  }
}

/** Function-style entry points for the same two combinational operations, using the SV cores.
  * The state bit layout matches rocket-chip's `PseudoLRU`, but only power-of-two way
  * counts are supported (the generator requires ways >= 2).
  */
object SvPLRU {
  def getReplaceWay(state: Bits)(implicit sourceInfo: SourceInfo): UInt =
    SvPLRUSelectTreeExplicit(state)

  def getNextState(state: Bits, touchWay: Bits)(implicit sourceInfo: SourceInfo): UInt =
    SvPLRUUpdate(state, touchWay)
}

