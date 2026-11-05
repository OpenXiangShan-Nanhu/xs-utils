package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}
import chisel3.util.{ArbiterIO, Decoupled, DecoupledIO, log2Ceil}
import xs.utils.arb.BaseArbiter

/** Shared VIP / round-robin core with an asynchronous active-high reset. */
class SvLivelockFreeArbiterImpl(
  dataWidth: Int = 32, inputSize: Int = 2, roundRobin: Boolean = false, selLatch: Boolean = false
)
    extends VlogExtModule(Map(
      "DATA_WIDTH" -> dataWidth, "INPUT_SIZE" -> inputSize,
      "ROUND_ROBIN" -> (if (roundRobin) 1 else 0), "SEL_LATCH" -> (if (selLatch) 1 else 0))) {
  require(dataWidth > 0, "SvLivelockFreeArbiter DATA_WIDTH must be positive")
  require(inputSize > 0, "SvLivelockFreeArbiter INPUT_SIZE must be positive")

  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))
  val io = IO(new Bundle {
    // Packed lane masks share the parameterized SV boundary; each bit is one lane.
    val in = new Bundle {
      val valid = Input(UInt(inputSize.W))
      val bits = Input(UInt((dataWidth * inputSize).W))
      val ready = Output(UInt(inputSize.W))
    }
    val out = Decoupled(UInt(dataWidth.W))
    val chosen = Output(UInt(math.max(1, log2Ceil(inputSize)).W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvLivelockFreeArbiterImpl)
  rtl.register(this, SvLivelockFreeArbiterImpl)
}

object SvLivelockFreeArbiterImpl extends VlogModule {
  override val moduleName = "SvLivelockFreeArbiter"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |`ifndef PRINTF_COND_
       |  `ifdef PRINTF_COND
       |    `define PRINTF_COND_ (`PRINTF_COND)
       |  `else
       |    `define PRINTF_COND_ 1
       |  `endif
       |`endif
       |`ifndef ASSERT_VERBOSE_COND_
       |  `ifdef ASSERT_VERBOSE_COND
       |    `define ASSERT_VERBOSE_COND_ (`ASSERT_VERBOSE_COND)
       |  `else
       |    `define ASSERT_VERBOSE_COND_ 1
       |  `endif
       |`endif
       |`ifndef STOP_COND_
       |  `ifdef STOP_COND
       |    `define STOP_COND_ (`STOP_COND)
       |  `else
       |    `define STOP_COND_ 1
       |  `endif
       |`endif
       |
       |module ${ctx.selfName} #(
       |  parameter integer DATA_WIDTH = 32,
       |  parameter integer INPUT_SIZE = 2,
       |  parameter integer ROUND_ROBIN = 0,
       |  parameter integer SEL_LATCH = 0
       |) (
       |  input  wire                                                       clock,
       |  input  wire                                                       reset,
       |  input  wire  [INPUT_SIZE - 1:0]                                   io_in_valid,
       |  input  wire  [INPUT_SIZE * DATA_WIDTH - 1:0]                      io_in_bits,
       |  output wire  [INPUT_SIZE - 1:0]                                   io_in_ready,
       |  output wire                                                       io_out_valid,
       |  output wire  [DATA_WIDTH - 1:0]                                   io_out_bits,
       |  input  wire                                                       io_out_ready,
       |  output wire  [((INPUT_SIZE > 1) ? $$clog2(INPUT_SIZE) : 1) - 1:0] io_chosen
       |);
       |  // VIP mode stores the preferred lane; RR mode stores the last fired lane.
       |  logic [INPUT_SIZE - 1:0] pointer;
       |  logic [INPUT_SIZE - 1:0] selection;
       |  wire [INPUT_SIZE - 1:0] selected;
       |  wire fire = io_out_valid && io_out_ready;
       |
       |  // Refill a registered selection using the priority after this transfer.
       |  wire select_after_fire;
       |  wire [INPUT_SIZE - 1:0] search_pointer = select_after_fire ? selected : pointer;
       |  wire [INPUT_SIZE - 1:0] higher_mask;
       |  ${ctx.ref(SvOHToMaskImpl)} #(.INPUT_SIZE(INPUT_SIZE), .HIGH_MASK(1)) u_higher_mask (
       |    .io_in(search_pointer), .io_out(higher_mask)
       |  );
       |
       |  wire [INPUT_SIZE - 1:0] higher_valid = io_in_valid & higher_mask;
       |  wire [INPUT_SIZE - 1:0] first_valid;
       |  wire [INPUT_SIZE - 1:0] first_higher;
       |  ${ctx.ref(SvPriorityEncoder1HImpl)} #(.INPUT_SIZE(INPUT_SIZE)) u_first_valid (
       |    .io_in(io_in_valid), .io_out(first_valid)
       |  );
       |  ${ctx.ref(SvPriorityEncoder1HImpl)} #(.INPUT_SIZE(INPUT_SIZE)) u_first_higher (
       |    .io_in(higher_valid), .io_out(first_higher)
       |  );
       |  // Search above the pointer, then wrap to the lowest valid lane.
       |  // A lone requester may win again; next-cycle valid prevents stale transfers.
       |  wire [INPUT_SIZE - 1:0] circular_choice = (|higher_valid) ? first_higher : first_valid;
       |
       |  // The policies share the search and differ only in grant / pointer updates.
       |  wire [INPUT_SIZE - 1:0] candidate;
       |  wire [INPUT_SIZE - 1:0] next_pointer;
       |  wire advance_pointer;
       |  wire pointer_valid = |(io_in_valid & search_pointer);
       |  wire other_valid = |(io_in_valid & ~search_pointer);
       |  // Chisel equivalent: circular_choice := Mux(higher_valid.orR, first_higher, first_valid)
       |  //   candidate := (if (ROUND_ROBIN != 0) circular_choice else
       |  //     Mux(select_after_fire, circular_choice, Mux(pointer_valid, search_pointer, first_valid)))
       |  generate
       |    if (ROUND_ROBIN != 0) begin : gen_round_robin
       |      assign candidate = circular_choice;
       |      assign next_pointer = selected;
       |      assign advance_pointer = fire;
       |    end else begin : gen_vip
       |      // Circular search feeds the next-cycle VIP, outside the grant path.
       |      assign candidate = select_after_fire ? circular_choice : (pointer_valid ? search_pointer : first_valid);
       |      assign next_pointer = circular_choice;
       |      assign advance_pointer = other_valid && (!pointer_valid || fire);
       |    end
       |  endgenerate
       |  // Chisel equivalent (clock, asynchronous reset): val pointer = RegInit(1.U(INPUT_SIZE.W))
       |  //   when(advance_pointer) { pointer := next_pointer }
       |  always_ff @(posedge clock or posedge reset) begin
       |    if (reset) pointer <= INPUT_SIZE'(1);
       |    else if (advance_pointer) pointer <= next_pointer;
       |  end
       |
       |  // Optional selection register: refill on transfer or when no grant is valid.
       |  generate
       |    if (SEL_LATCH != 0) begin : gen_sel_latch
       |      assign select_after_fire = fire;
       |      // Chisel equivalent (clock, asynchronous reset): val selection = RegInit(0.U(INPUT_SIZE.W))
       |      //   when(!io_out_valid || io_out_ready) { selection := candidate }; selected := selection & io_in_valid
       |      always_ff @(posedge clock or posedge reset) begin
       |        if (reset) selection <= '0;
       |        else if (!io_out_valid || io_out_ready) selection <= candidate;
       |      end
       |      // Only the choice is stored. Gate withdrawn/consumed requests so
       |      // an obsolete choice cannot cause a phantom transfer.
       |      assign selected = selection & io_in_valid;
       |      assign io_out_valid = |selected;
       |    end else begin : gen_sel_comb
       |      assign select_after_fire = 1'b0;
       |      assign selected = candidate;
       |      assign io_out_valid = |io_in_valid;
       |    end
       |  endgenerate
       |
       |  assign io_in_ready = selected & {INPUT_SIZE{io_out_ready}};
       |  // Chisel equivalent: io_out_bits := (0 until INPUT_SIZE).map(i =>
       |  //   Mux(selected(i), io_in_bits((i + 1) * DATA_WIDTH - 1, i * DATA_WIDTH), 0.U(DATA_WIDTH.W))).reduce(_ | _)
       |  ${ctx.ref(SvMux1HImpl)} #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(INPUT_SIZE)) u_out_bits (
       |    .io_sel(selected), .io_in(io_in_bits), .io_out(io_out_bits)
       |  );
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(INPUT_SIZE)) u_chosen (
       |    .io_in(selected), .io_out(io_chosen)
       |  );
       |
       |`ifndef SYNTHESIS
       |  always @(posedge clock) begin
       |    if (!reset && !$$onehot(pointer)) begin
       |      if (`PRINTF_COND_)
       |        $$fwrite(32'h80000002,
       |          "Assertion failed at %0t in %m: SvLivelockFreeArbiter pointer must be one-hot (pointer=%b)\\n",
       |          $$time, pointer);
       |      if (`ASSERT_VERBOSE_COND_) $$error;
       |      if (`STOP_COND_) $$fatal;
       |    end
       |    if (!reset && advance_pointer && !($$onehot(next_pointer) && (|(next_pointer & io_in_valid)))) begin
       |      if (`PRINTF_COND_)
       |        $$fwrite(32'h80000002,
       |          "Assertion failed at %0t in %m: SvLivelockFreeArbiter next_pointer must select one valid input (next_pointer=%b, valid=%b)\\n",
       |          $$time, next_pointer, io_in_valid);
       |      if (`ASSERT_VERBOSE_COND_) $$error;
       |      if (`STOP_COND_) $$fatal;
       |    end
       |  end
       |`endif
       |endmodule
       |""".stripMargin
}

/** Common inline Chisel adapter. Only the selected valid input is ready;
  * idle bits/chosen are zero, including INPUT_SIZE=1. Reset is asynchronous.
  * Both policies serve a continuously pending lane within inputSize transfers
  * if reset stays inactive and the sink keeps accepting. This is Decoupled,
  * not Irrevocable: producers remain responsible for their payloads.
  * selLatch registers only the choice, adds one cycle of selection latency,
  * and holds a valid grant under backpressure. Continuous requests sustain
  * one transfer per cycle, including a single requester. Newly arriving or
  * withdrawn requests may introduce a selection bubble; no payload is buffered.
  */
class SvLivelockFreeArbiter[T <: Data](
  gen: T, inputSize: Int, roundRobin: Boolean = false, selLatch: Boolean = false
) extends BaseArbiter(gen, inputSize) {
  require(inputSize > 0, "SvLivelockFreeArbiter requires at least one input")
  require(gen.isWidthKnown && gen.getWidth > 0,
    "SvLivelockFreeArbiter requires a known, positive data width")
  override def desiredName: String = "SvLivelockFreeArbiterWrapper"

  private val core = Module(new SvLivelockFreeArbiterImpl(gen.getWidth, inputSize, roundRobin, selLatch))
  core.clock := clock
  core.reset := reset.asAsyncReset
  core.io.in.valid := VecInit(io.in.map(_.valid)).asUInt
  core.io.in.bits := VecInit(io.in.map(_.bits)).asUInt
  core.io.out.ready := io.out.ready
  io.out.valid := core.io.out.valid
  io.out.bits := core.io.out.bits.asTypeOf(gen)
  io.chosen := core.io.chosen
  io.in.zip(core.io.in.ready.asBools).foreach { case (lane, ready) => lane.ready := ready }

  inlineInstance(this)
}

// Keep the defaulted Data overload below the sequence overloads in priority:
// Chisel Vec is both Data and Seq, including the result of Vec.toSeq.
private[verilog] trait SvLivelockFreeArbiterConfigFactory {
  def apply[T <: Data](
    gen: T = UInt(32.W), inputSize: Int = 2, roundRobin: Boolean = false, selLatch: Boolean = false
  )(
    implicit sourceInfo: SourceInfo
  ): ArbiterIO[T] = Module(new SvLivelockFreeArbiter(gen, inputSize, roundRobin, selLatch)).io
}

object SvLivelockFreeArbiter extends SvLivelockFreeArbiterConfigFactory {
  def apply[T <: Data](in: Seq[DecoupledIO[T]])(implicit sourceInfo: SourceInfo): DecoupledIO[T] =
    apply(in, roundRobin = false)

  def apply[T <: Data](in: Seq[DecoupledIO[T]], roundRobin: Boolean)(
    implicit sourceInfo: SourceInfo
  ): DecoupledIO[T] = apply(in, roundRobin, selLatch = false)

  def apply[T <: Data](in: Seq[DecoupledIO[T]], roundRobin: Boolean, selLatch: Boolean)(
    implicit sourceInfo: SourceInfo
  ): DecoupledIO[T] = {
    require(in.nonEmpty, "SvLivelockFreeArbiter requires non-empty inputs")
    in.foreach(lane => requireIsHardware(lane, "SvLivelockFreeArbiter input"))
    val arb = apply(chiselTypeOf(in.head.bits), in.size, roundRobin, selLatch)
    arb.in.zip(in).foreach { case (sink, source) => sink <> source }
    arb.out
  }
}

/** Typed VIP view of the shared SvLivelockFreeArbiter core. */
class SvVipArbiter[T <: Data](gen: T, size: Int, selLatch: Boolean = false)
    extends SvLivelockFreeArbiter(gen, size, roundRobin = false, selLatch = selLatch) {
  override def desiredName: String = "SvVipArbiterWrapper"
}

// Lower priority than sequence overloads when the input is a Chisel Vec.
private[verilog] trait SvVipArbiterConfigFactory {
  def apply[T <: Data](gen: T = UInt(32.W), size: Int = 2, selLatch: Boolean = false)(
    implicit sourceInfo: SourceInfo
  ): ArbiterIO[T] = Module(new SvVipArbiter(gen, size, selLatch)).io
}

object SvVipArbiter extends SvVipArbiterConfigFactory {
  def apply[T <: Data](in: Seq[DecoupledIO[T]])(implicit sourceInfo: SourceInfo): DecoupledIO[T] =
    apply(in, selLatch = false)

  def apply[T <: Data](in: Seq[DecoupledIO[T]], selLatch: Boolean)(
    implicit sourceInfo: SourceInfo
  ): DecoupledIO[T] = SvLivelockFreeArbiter(in, roundRobin = false, selLatch = selLatch)
}

/** Typed round-robin view of the shared SvLivelockFreeArbiter core. */
class SvRRArbiter[T <: Data](gen: T, n: Int, selLatch: Boolean = false)
    extends SvLivelockFreeArbiter(gen, n, roundRobin = true, selLatch = selLatch) {
  override def desiredName: String = "SvRRArbiterWrapper"
}

// Lower priority than sequence overloads when the input is a Chisel Vec.
private[verilog] trait SvRRArbiterConfigFactory {
  def apply[T <: Data](gen: T = UInt(32.W), n: Int = 2, selLatch: Boolean = false)(
    implicit sourceInfo: SourceInfo
  ): ArbiterIO[T] = Module(new SvRRArbiter(gen, n, selLatch)).io
}

object SvRRArbiter extends SvRRArbiterConfigFactory {
  def apply[T <: Data](in: Seq[DecoupledIO[T]])(implicit sourceInfo: SourceInfo): DecoupledIO[T] =
    apply(in, selLatch = false)

  def apply[T <: Data](in: Seq[DecoupledIO[T]], selLatch: Boolean)(
    implicit sourceInfo: SourceInfo
  ): DecoupledIO[T] = SvLivelockFreeArbiter(in, roundRobin = true, selLatch = selLatch)
}
