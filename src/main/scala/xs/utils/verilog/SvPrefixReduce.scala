package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}

/** Inclusive bit-prefix reduction: Scala op 0 = OR, 1 = AND, 2 = XOR.
  * The first parameter list selects the Scala template; the second supplies SV instance parameters.
  * HIGH_PREFIX=0 reduces bits [i:0]; HIGH_PREFIX=1 reduces bits [INPUT_SIZE-1:i].
  */
class SvPrefixReduceImpl(op: Int = 0)(inputSize: Int = 32, highPrefix: Boolean = false) extends VlogExtModule(Map(
  "INPUT_SIZE" -> inputSize,
  "HIGH_PREFIX" -> (if (highPrefix) 1 else 0)
)) {
  require(inputSize > 0, "SvPrefixReduce INPUT_SIZE must be positive")
  require(op >= 0 && op <= 2, "SvPrefixReduce op must be 0 (OR), 1 (AND), or 2 (XOR)")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(inputSize.W))
  })

  private val template = SvPrefixReduceImpl(op)
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(template)
  rtl.register(this, template)
}

object SvPrefixReduceImpl extends VlogModuleProvider {
  override def modules: Seq[VlogModule] = (0 to 2).map(apply)

  def apply(op: Int = 0): VlogModule = {
    val (suffix, operator, identity) = op match {
      case 0 => ("OR", "|", false)
      case 1 => ("AND", "&", true)
      case 2 => ("XOR", "^", false)
      case _ => throw new IllegalArgumentException("SvPrefixReduce op must be 0 (OR), 1 (AND), or 2 (XOR)")
    }
    VlogModule(s"SvPrefixReduce$suffix")(render(operator, identity))
  }

  private def render(operator: String, identity: Boolean)(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |
       |// HIGH_PREFIX == 0: io_out[i] = $operator io_in[i:0];
       |// HIGH_PREFIX != 0: io_out[i] = $operator io_in[INPUT_SIZE - 1:i];
       |
       |// Example: INPUT_SIZE=8, HIGH_PREFIX=0; LEVELS=3, LEAF_COUNT=8, padded_input=io_in.
       |// level_offset(0)=0, level_offset(1)=1, level_offset(2)=3; partial_result has 7 bits.
       |// The packed tree is indexed breadth-first, but computed from level 2 up to level 0:
       |//   Level 0:
       |//     partial_result[0] = partial_result[1] $operator partial_result[2] = $operator io_in[7:0];
       |//   Level 1:
       |//     partial_result[1] = partial_result[3] $operator partial_result[4] = $operator io_in[3:0];
       |//     partial_result[2] = partial_result[5] $operator partial_result[6] = $operator io_in[7:4];
       |//   Level 2:
       |//     partial_result[3] = io_in[0] $operator io_in[1] = $operator io_in[1:0];
       |//     partial_result[4] = io_in[2] $operator io_in[3] = $operator io_in[3:2];
       |//     partial_result[5] = io_in[4] $operator io_in[5] = $operator io_in[5:4];
       |//     partial_result[6] = io_in[6] $operator io_in[7] = $operator io_in[7:6];
       |// Each output combines disjoint intervals; neutral terms are omitted below:
       |//   io_out[0] = io_in[0];
       |//   io_out[1] = partial_result[3];
       |//   io_out[2] = partial_result[3] $operator io_in[2];
       |//   io_out[3] = partial_result[1];
       |//   io_out[4] = partial_result[1] $operator io_in[4];
       |//   io_out[5] = partial_result[1] $operator partial_result[5];
       |//   io_out[6] = partial_result[1] $operator partial_result[5] $operator io_in[6];
       |//   io_out[7] = partial_result[0];
       |// For example, prefix length 7 = 4 + 2 + 1 selects [3:0], [5:4], and bit 6.
       |// In packed order [3:0], prefix_terms[6] = {io_in[6], partial_result[5], partial_result[1], IDENTITY};
       |// io_out[6] = $operator prefix_terms[6]; IDENTITY is 1'b${if (identity) 1 else 0} for this operation.
       |// These are inclusive prefixes, with no overlapping intervals (also required for XOR).
       |//
       |// For INPUT_SIZE=8, HIGH_PREFIX!=0, the same tree uses reversed leaves and output indices:
       |//   padded_input[j] = io_in[7-j]; io_out[i] = reduced_bits[7-i].
       |//   partial_result[1] = $operator io_in[7:4]; partial_result[5] = $operator io_in[3:2];
       |//   io_out[7] = io_in[7]; io_out[6] = partial_result[3];
       |//   io_out[1] = partial_result[1] $operator partial_result[5] $operator io_in[1];
       |//   io_out[0] = partial_result[0] = $operator io_in[7:0].
       |
       |module ${ctx.selfName} #(
       |  parameter integer INPUT_SIZE = 32,
       |  parameter integer HIGH_PREFIX = 0
       |) (
       |  input  wire  [INPUT_SIZE - 1:0] io_in,
       |  output logic [INPUT_SIZE - 1:0] io_out
       |);
       |  // One dummy tree level avoids a zero-width partial_result for INPUT_SIZE=1.
       |  // Its output still reduces the single input directly; the unused node is removable.
       |  localparam integer LEVELS = (INPUT_SIZE > 1) ? $$clog2(INPUT_SIZE) : 1;
       |  localparam integer LEAF_COUNT = 1 << LEVELS;
       |  localparam bit IDENTITY = 1'b${if (identity) 1 else 0};
       |
       |  // Breadth-first tree layout: level L starts after 1 + 2 + ... + 2**(L-1) nodes.
       |  function automatic integer level_offset(input integer level);
       |    level_offset = (1 << level) - 1;
       |  endfunction
       |
       |  logic [level_offset(LEVELS) - 1:0] partial_result;
       |  logic [LEAF_COUNT - 1:0] padded_input;
       |  logic [INPUT_SIZE - 1:0] reduced_bits;
       |  logic [LEVELS:0] prefix_terms [INPUT_SIZE];
       |
       |  // Chisel equivalent: padded_input.take(INPUT_SIZE) := (if (HIGH_PREFIX != 0) Reverse(io_in) else io_in)
       |  //   io_out := (if (HIGH_PREFIX != 0) Reverse(reduced_bits) else reduced_bits)
       |  generate
       |    if (HIGH_PREFIX != 0) begin : gen_high_prefix
       |      always_comb begin
       |        for (integer i = 0; i < LEAF_COUNT; i = i + 1) begin : prepare_leaf
       |          if (i < INPUT_SIZE)
       |            padded_input[i] = io_in[INPUT_SIZE - 1 - i];
       |          else
       |            padded_input[i] = IDENTITY;
       |        end
       |      end
       |      // Keep output wiring separate from input wiring to avoid artificial process-level cycles.
       |      always_comb begin
       |        for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : reverse_output
       |          io_out[i] = reduced_bits[INPUT_SIZE - 1 - i];
       |        end
       |      end
       |    end else begin : gen_low_prefix
       |      always_comb begin
       |        for (integer i = 0; i < LEAF_COUNT; i = i + 1) begin : prepare_leaf
       |          if (i < INPUT_SIZE)
       |            padded_input[i] = io_in[i];
       |          else
       |            padded_input[i] = IDENTITY;
       |        end
       |      end
       |      assign io_out = reduced_bits;
       |    end
       |  endgenerate
       |
       |  // Node (L, B) reduces the aligned leaf interval
       |  // [B * 2**(LEVELS-L), (B+1) * 2**(LEVELS-L) - 1]. Root: (0, 0).
       |  // Chisel-style structure: partial_result is padded_input.reduceTree(combine), retaining each node.
       |  // Descending levels visit every child before its parent; this is still a balanced tree, not a scan chain.
       |  // Decompose prefix length i+1 into disjoint power-of-two intervals of size S=2**(LEVELS-L).
       |  // Chisel equivalent: terms(L) := (if (((i+1) & S) != 0) partial_result(level_offset(L)+(i+1)/S-1) else identity)
       |  //   reduced_bits(i) := terms.reduce(combine); the size-one term is padded_input(i) for odd lengths.
       |  // prefix_terms contains only tree/leaf references and identity constants; XOR never double-counts a bit.
       |  // combine = (_ $operator _); operator and identity are selected by Scala.
       |  always_comb begin
       |    for (integer level = LEVELS - 1; level >= 0; level = level - 1) begin : calc_level
       |      for (integer bit_index = 0; bit_index < (1 << level); bit_index = bit_index + 1) begin : calc_node
       |        if (level == LEVELS - 1) begin
       |          partial_result[level_offset(level) + bit_index] =
       |            padded_input[2 * bit_index] $operator padded_input[2 * bit_index + 1];
       |        end else begin
       |          partial_result[level_offset(level) + bit_index] =
       |            partial_result[level_offset(level + 1) + 2 * bit_index] $operator
       |            partial_result[level_offset(level + 1) + 2 * bit_index + 1];
       |        end
       |      end
       |    end
       |    for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : calc_prefix
       |      for (integer level = 0; level < LEVELS; level = level + 1) begin : select_term
       |        if (((i + 1) & (1 << (LEVELS - level))) != 0)
       |          prefix_terms[i][level] = partial_result[level_offset(level) + ((i + 1) >> (LEVELS - level)) - 1];
       |        else
       |          prefix_terms[i][level] = IDENTITY;
       |      end
       |      if (((i + 1) & 1) != 0)
       |        prefix_terms[i][LEVELS] = padded_input[i];
       |      else
       |        prefix_terms[i][LEVELS] = IDENTITY;
       |      reduced_bits[i] = $operator prefix_terms[i];
       |    end
       |  end
       |endmodule
       |""".stripMargin
}

/** Inline wrapper for an inclusive prefix built from shared balanced interval reductions. */
class SvPrefixReduce(inputSize: Int = 32, highPrefix: Boolean = false, op: Int = 0) extends RawModule {
  require(inputSize > 0, "SvPrefixReduce requires a positive input size")
  require(op >= 0 && op <= 2, "SvPrefixReduce op must be 0 (OR), 1 (AND), or 2 (XOR)")

  val io = IO(new Bundle {
    val in = Input(UInt(inputSize.W))
    val out = Output(UInt(inputSize.W))
  })

  private val core = Module(new SvPrefixReduceImpl(op)(inputSize, highPrefix))
  core.io.in := io.in
  io.out := core.io.out
  inlineInstance(this)
}

object SvPrefixReduce {
  final val Or = 0
  final val And = 1
  final val Xor = 2

  /** Infer a positive, known width from hardware Bits. A singleton needs no SV core. */
  def apply(in: Bits, highPrefix: Boolean = false, op: Int = 0)(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(in, "SvPrefixReduce input")
    require(in.isWidthKnown && in.getWidth > 0, "SvPrefixReduce requires a known, positive input width")
    require(op >= Or && op <= Xor, "SvPrefixReduce op must be 0 (OR), 1 (AND), or 2 (XOR)")
    if (in.getWidth == 1) in.asUInt
    else {
      val reducer = Module(new SvPrefixReduce(in.getWidth, highPrefix, op))
      reducer.io.in := in.asUInt
      reducer.io.out
    }
  }
}
