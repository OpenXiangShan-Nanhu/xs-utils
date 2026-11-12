package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}
import chisel3.util.{ArbiterIO, Decoupled, DecoupledIO, log2Ceil}

/** Shared VIP / round-robin core with an asynchronous active-high reset. */
class SvLivelockFreeArbiterImpl(
  dataWidth: Int = 32, n: Int = 2, roundRobin: Boolean = false
) extends VlogExtModule(Map(
  "DATA_WIDTH" -> dataWidth, "INPUT_SIZE" -> n,
  "ROUND_ROBIN" -> (if (roundRobin) 1 else 0)
)) {
  require(dataWidth > 0, "SvLivelockFreeArbiter DATA_WIDTH must be positive")
  require(n > 0, "SvLivelockFreeArbiter INPUT_SIZE must be positive")

  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))
  val io = IO(new Bundle {
    // Packed lane masks share the parameterized SV boundary; each bit is one lane.
    val in = new Bundle {
      val valid = Input(UInt(n.W))
      val bits = Input(UInt((dataWidth * n).W))
      val ready = Output(UInt(n.W))
    }
    val out = Decoupled(UInt(dataWidth.W))
    val chosen = Output(UInt(math.max(1, log2Ceil(n)).W))
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
       |  parameter integer ROUND_ROBIN = 0
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
       |  // All state uses the shared asynchronous active-high reset.
       |  logic [INPUT_SIZE - 1:0] pointer;
       |  wire [INPUT_SIZE - 1:0] selected;
       |  wire fire = io_out_valid && io_out_ready;
       |  wire [INPUT_SIZE - 1:0] higher_mask;
       |  wire [INPUT_SIZE - 1:0] priority_valid;
       |  wire [INPUT_SIZE - 1:0] first_valid;
       |  wire [INPUT_SIZE - 1:0] first_priority;
       |  wire [INPUT_SIZE - 1:0] circular_choice;
       |  wire [INPUT_SIZE - 1:0] next_pointer;
       |  wire advance_pointer;
       |  wire pointer_valid;
       |  wire other_valid;
       |
       |  ${ctx.ref(SvOHToMaskImpl)} #(.INPUT_SIZE(INPUT_SIZE), .HIGH_MASK(1)) u_higher_mask (
       |    .io_in(pointer), .io_out(higher_mask)
       |  );
       |  ${ctx.ref(SvPriorityEncoder1HImpl)} #(.INPUT_SIZE(INPUT_SIZE)) u_first_valid (
       |    .io_in(io_in_valid), .io_out(first_valid)
       |  );
       |  ${ctx.ref(SvPriorityEncoder1HImpl)} #(.INPUT_SIZE(INPUT_SIZE)) u_first_priority (
       |    .io_in(priority_valid), .io_out(first_priority)
       |  );
       |
       |  assign priority_valid = io_in_valid & higher_mask;
       |  assign pointer_valid = |(io_in_valid & pointer);
       |  assign other_valid = |(io_in_valid & ~pointer);
       |  assign circular_choice = (|priority_valid) ? first_priority : first_valid;
       |
       |  generate
       |    if (ROUND_ROBIN != 0) begin : gen_round_robin
       |      assign selected = circular_choice;
       |      assign next_pointer = selected;
       |      assign advance_pointer = fire;
       |    end else begin : gen_vip
       |      // Circular search feeds the next-cycle VIP, outside the grant path.
       |      assign selected = pointer_valid ? pointer : first_valid;
       |      assign next_pointer = circular_choice;
       |      assign advance_pointer = other_valid && (!pointer_valid || fire);
       |    end
       |    // Chisel equivalent (clock, asynchronous reset): val pointer = RegInit(1.U(INPUT_SIZE.W))
       |    //   when(advance_pointer) { pointer := next_pointer }
       |    always_ff @(posedge clock or posedge reset) begin
       |      if (reset) pointer <= INPUT_SIZE'(1);
       |      else if (advance_pointer) pointer <= next_pointer;
       |    end
       |
       |`ifndef SYNTHESIS
       |    always @(posedge clock) begin
       |      if (!reset && !$$onehot(pointer)) begin
       |        if (`PRINTF_COND_)
       |          $$fwrite(32'h80000002,
       |            "Assertion failed at %0t in %m: SvLivelockFreeArbiter pointer must be one-hot (pointer=%b)\\n",
       |            $$time, pointer);
       |        if (`ASSERT_VERBOSE_COND_) $$error;
       |        if (`STOP_COND_) $$fatal;
       |      end
       |      if (!reset && advance_pointer && !($$onehot(next_pointer) && (|(next_pointer & io_in_valid)))) begin
       |        if (`PRINTF_COND_)
       |          $$fwrite(32'h80000002,
       |            "Assertion failed at %0t in %m: SvLivelockFreeArbiter next_pointer must select one valid input (next_pointer=%b, valid=%b)\\n",
       |            $$time, next_pointer, io_in_valid);
       |        if (`ASSERT_VERBOSE_COND_) $$error;
       |        if (`STOP_COND_) $$fatal;
       |      end
       |    end
       |`endif
       |  endgenerate
       |
       |  assign io_out_valid = |io_in_valid;
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
       |endmodule
       |""".stripMargin
}

/** Common inline Chisel adapter. With one input, valid/bits/ready pass through
  * without a core or additional latency; chosen is zero even when idle. For
  * multiple inputs, only the selected valid input is ready and idle bits are zero.
  * The multi-input core has an asynchronous reset.
  * Both policies serve a continuously pending lane within n transfers if
  * reset stays inactive and the sink keeps accepting. This is Decoupled,
  * not Irrevocable; no selection or payload pipeline is added.
  */
class SvLivelockFreeArbiter[T <: Data](
  gen: T, n: Int, roundRobin: Boolean = false
) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  require(n > 0, "SvLivelockFreeArbiter requires at least one input")
  val io = IO(new ArbiterIO(gen, n))
  override def desiredName: String = "SvLivelockFreeArbiterWrapper"

  val core = Option.when(n > 1) {
    require(gen.isWidthKnown && gen.getWidth > 0,
      "SvLivelockFreeArbiter requires a known, positive data width when n > 1")
    Module(new SvLivelockFreeArbiterImpl(gen.getWidth, n, roundRobin))
  }

  if (n == 1) {
    io.out <> io.in.head
    io.chosen := 0.U
  } else {
    core.get.clock := clock
    core.get.reset := reset.asAsyncReset
    core.get.io.in.valid := VecInit(io.in.map(_.valid)).asUInt
    core.get.io.in.bits := VecInit(io.in.map(_.bits)).asUInt
    core.get.io.out.ready := io.out.ready
    io.out.valid := core.get.io.out.valid
    io.out.bits := core.get.io.out.bits.asTypeOf(gen)
    io.chosen := core.get.io.chosen
    io.in.zip(core.get.io.in.ready.asBools).foreach { case (lane, ready) => lane.ready := ready }
  }

  inlineInstance(this)
}

// Keep the defaulted Data overload below the sequence overloads in priority:
// Chisel Vec is both Data and Seq, including the result of Vec.toSeq.
private[verilog] trait SvLivelockFreeArbiterConfigFactory {
  /** Single-input io needs no wrapper or SV core; allow inferred payload widths. */
  def apply[T <: Data](
    gen: T = UInt(32.W), n: Int = 2, roundRobin: Boolean = false
  )(
    implicit sourceInfo: SourceInfo
  ): ArbiterIO[T] = {
    require(n > 0, "SvLivelockFreeArbiter requires at least one input")
    if (n == 1) {
      val io = Wire(new ArbiterIO(gen, n))
      io.out <> io.in.head
      io.chosen := 0.U
      io
    } else {
      Module(new SvLivelockFreeArbiter(gen, n, roundRobin)).io
    }
  }
}

object SvLivelockFreeArbiter extends SvLivelockFreeArbiterConfigFactory {
  def apply[T <: Data](in: Seq[DecoupledIO[T]])(implicit sourceInfo: SourceInfo): DecoupledIO[T] =
    apply(in, roundRobin = false)

  def apply[T <: Data](in: Seq[DecoupledIO[T]], roundRobin: Boolean)(
    implicit sourceInfo: SourceInfo
  ): DecoupledIO[T] = {
    require(in.nonEmpty, "SvLivelockFreeArbiter requires non-empty inputs")
    in.foreach(lane => requireIsHardware(lane, "SvLivelockFreeArbiter input"))
    if (in.size == 1) in.head
    else {
      val arb = apply(chiselTypeOf(in.head.bits), in.size, roundRobin)
      arb.in.zip(in).foreach { case (sink, source) => sink <> source }
      arb.out
    }
  }
}

/** VIP policy of the livelock-free core. */
class SvVipArbiter[T <: Data](gen: T, size: Int)
    extends SvLivelockFreeArbiter(gen, size, roundRobin = false) {
  override def desiredName: String = "SvVipArbiterWrapper"
}

object SvVipArbiter {
  def apply[T <: Data](gen: T = UInt(32.W), size: Int = 2)(
    implicit sourceInfo: SourceInfo
  ): ArbiterIO[T] = Module(new SvVipArbiter(gen, size)).io

  def apply[T <: Data](in: Seq[DecoupledIO[T]])(implicit sourceInfo: SourceInfo): DecoupledIO[T] =
    SvLivelockFreeArbiter(in, roundRobin = false)
}

/** Round-robin policy of the livelock-free core. */
class SvRRArbiter[T <: Data](gen: T, n: Int)
    extends SvLivelockFreeArbiter(gen, n, roundRobin = true) {
  override def desiredName: String = "SvRRArbiterWrapper"
}

object SvRRArbiter {
  def apply[T <: Data](gen: T = UInt(32.W), n: Int = 2)(
    implicit sourceInfo: SourceInfo
  ): ArbiterIO[T] = Module(new SvRRArbiter(gen, n)).io

  def apply[T <: Data](in: Seq[DecoupledIO[T]])(implicit sourceInfo: SourceInfo): DecoupledIO[T] =
    SvLivelockFreeArbiter(in, roundRobin = true)
}
