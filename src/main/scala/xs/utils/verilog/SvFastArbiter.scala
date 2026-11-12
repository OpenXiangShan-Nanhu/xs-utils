package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}
import chisel3.util.{ArbiterIO, Decoupled, DecoupledIO, log2Ceil}

/** Historical-pending-request arbiter with an asynchronous active-high reset. */
class SvFastArbiterImpl(
  dataWidth: Int = 32, n: Int = 2
) extends VlogExtModule(Map(
  "DATA_WIDTH" -> dataWidth, "INPUT_SIZE" -> n
)) {
  require(dataWidth > 0, "SvFastArbiter DATA_WIDTH must be positive")
  require(n > 0, "SvFastArbiter INPUT_SIZE must be positive")

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
  override val desiredName: String = rtl.nameOf(SvFastArbiterImpl)
  rtl.register(this, SvFastArbiterImpl)
}

object SvFastArbiterImpl extends VlogModule {
  override val moduleName = "SvFastArbiter"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer DATA_WIDTH = 32,
       |  parameter integer INPUT_SIZE = 2
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
       |  logic [INPUT_SIZE - 1:0] pending_mask;
       |  logic [INPUT_SIZE - 1:0] rr_grant_mask;
       |  wire [INPUT_SIZE - 1:0] selected;
       |  wire fire = io_out_valid && io_out_ready;
       |  wire [INPUT_SIZE - 1:0] higher_mask;
       |  wire [INPUT_SIZE - 1:0] priority_valid;
       |  wire [INPUT_SIZE - 1:0] first_valid;
       |  wire [INPUT_SIZE - 1:0] first_priority;
       |
       |  ${ctx.ref(SvOHToMaskImpl)} #(.INPUT_SIZE(INPUT_SIZE), .HIGH_MASK(1)) u_higher_mask (
       |    .io_in(selected), .io_out(higher_mask)
       |  );
       |  ${ctx.ref(SvPriorityEncoder1HImpl)} #(.INPUT_SIZE(INPUT_SIZE)) u_first_valid (
       |    .io_in(io_in_valid), .io_out(first_valid)
       |  );
       |  ${ctx.ref(SvPriorityEncoder1HImpl)} #(.INPUT_SIZE(INPUT_SIZE)) u_first_priority (
       |    .io_in(priority_valid), .io_out(first_priority)
       |  );
       |
       |  assign priority_valid = rr_grant_mask & pending_mask;
       |  // Chisel equivalent: val rrSelOH = PriorityEncoderOH(rr_grant_mask & pending_mask)
       |  //   selected := Mux((rrSelOH & io_in_valid).orR, rrSelOH, PriorityEncoderOH(io_in_valid))
       |  // If the first historical request withdrew, fall back directly to
       |  // the lowest live request; do not skip to another historical request.
       |  assign selected = (|(first_priority & io_in_valid)) ? first_priority : first_valid;
       |  // Chisel equivalent (clock, asynchronous reset):
       |  //   pending_mask := RegEnable(io_in_valid & ~selected, 0.U(INPUT_SIZE.W), fire)
       |  //   rr_grant_mask := RegEnable(higher_mask, 0.U(INPUT_SIZE.W), fire)
       |  always_ff @(posedge clock or posedge reset) begin
       |    if (reset) begin
       |      pending_mask <= '0;
       |      rr_grant_mask <= '0;
       |    end else if (fire) begin
       |      pending_mask <= io_in_valid & ~selected;
       |      rr_grant_mask <= higher_mask;
       |    end
       |  end
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

/** Independent FAST adapter, without a selection or payload pipeline.
  * Prefer the first higher-index request pending at the previous transfer if
  * still valid; otherwise choose the lowest live request. This is Decoupled,
  * not Irrevocable. A singleton bypasses the SV core and supports inferred widths.
  */
class SvFastArbiter[T <: Data](
  val gen: T, val n: Int
) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  require(n > 0, "SvFastArbiter requires at least one input")
  val io = IO(new ArbiterIO(gen, n))
  override def desiredName: String = "SvFastArbiterWrapper"

  val core = Option.when(n > 1) {
    require(gen.isWidthKnown && gen.getWidth > 0,
      "SvFastArbiter requires a known, positive data width when n > 1")
    Module(new SvFastArbiterImpl(gen.getWidth, n))
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

object SvFastArbiter {
  def apply[T <: Data](gen: T = UInt(32.W), n: Int = 2)(
    implicit sourceInfo: SourceInfo
  ): ArbiterIO[T] = Module(new SvFastArbiter(gen, n)).io

  def apply[T <: Data](in: Seq[DecoupledIO[T]])(implicit sourceInfo: SourceInfo): DecoupledIO[T] = {
    require(in.nonEmpty, "SvFastArbiter requires non-empty inputs")
    in.foreach(lane => requireIsHardware(lane, "SvFastArbiter input"))
    if (in.size == 1) in.head
    else {
      val arb = apply(chiselTypeOf(in.head.bits), in.size)
      arb.in.zip(in).foreach { case (sink, source) => sink <> source }
      arb.out
    }
  }

  // Preserve the existing FastArbiter(in, out, name) connection API.
  def apply[T <: Data](in: Seq[DecoupledIO[T]], out: DecoupledIO[T])(
    implicit sourceInfo: SourceInfo
  ): Unit = apply(in, out, None)

  def apply[T <: Data](in: Seq[DecoupledIO[T]], out: DecoupledIO[T], name: Option[String])(
    implicit sourceInfo: SourceInfo
  ): Unit = {
    val arb = Module(new SvFastArbiter(chiselTypeOf(out.bits), in.size))
    name.foreach(value => arb.suggestName(s"${value}_arb"))
    arb.io.in.zip(in).foreach { case (sink, source) => sink <> source }
    out <> arb.io.out
  }
}

/** FAST arbitration followed by the existing depth-one queue (no pipe or flow).
  * Enqueue acknowledges the selected input and stores both payload and choice.
  * The stored output survives input withdrawal until consumed; no same-cycle
  * refill, including n == 1. Valid resets asynchronously; data is unreset and
  * unspecified when invalid, as in SvFastQueueD1.
  */
class SvLatchFastArbiter[T <: Data](gen: T, n: Int) extends SvFastArbiter(gen, n) {
  override def desiredName: String = "SvLatchFastArbiterWrapper"
  require(gen.isWidthKnown && gen.getWidth > 0,
    "SvLatchFastArbiter requires a known, positive data width")

  private val latch = Module(new SvFastQueue(new Bundle {
    val dat = gen.cloneType
    val chosen = UInt(io.chosen.getWidth.W)
  }, 1, pipe = false))

  io.out.valid := latch.io.deq.valid
  io.out.bits := latch.io.deq.bits.dat
  io.chosen := latch.io.deq.bits.chosen
  latch.io.deq.ready := io.out.ready
  if (n == 1) {
    latch.io.enq.valid := io.in.head.valid
    latch.io.enq.bits.dat := io.in.head.bits
    latch.io.enq.bits.chosen := 0.U
    io.in.head.ready := latch.io.enq.ready
  } else {
    latch.io.enq.valid := core.get.io.out.valid
    latch.io.enq.bits.dat := core.get.io.out.bits.asTypeOf(gen)
    latch.io.enq.bits.chosen := core.get.io.chosen
    core.get.io.out.ready := latch.io.enq.ready
  }
}

object SvLatchFastArbiter {
  def apply[T <: Data](gen: T = UInt(32.W), n: Int = 2)(
    implicit sourceInfo: SourceInfo
  ): ArbiterIO[T] = Module(new SvLatchFastArbiter(gen, n)).io

  def apply[T <: Data](in: Seq[DecoupledIO[T]])(implicit sourceInfo: SourceInfo): DecoupledIO[T] = {
    require(in.nonEmpty, "SvLatchFastArbiter requires non-empty inputs")
    in.foreach(lane => requireIsHardware(lane, "SvLatchFastArbiter input"))
    val arb = apply(chiselTypeOf(in.head.bits), in.size)
    arb.in.zip(in).foreach { case (sink, source) => sink <> source }
    arb.out
  }

  def apply[T <: Data](in: Seq[DecoupledIO[T]], out: DecoupledIO[T])(
    implicit sourceInfo: SourceInfo
  ): Unit = apply(in, out, None)

  def apply[T <: Data](in: Seq[DecoupledIO[T]], out: DecoupledIO[T], name: Option[String])(
    implicit sourceInfo: SourceInfo
  ): Unit = {
    val arb = Module(new SvLatchFastArbiter(chiselTypeOf(out.bits), in.size))
    name.foreach(value => arb.suggestName(s"${value}_arb"))
    arb.io.in.zip(in).foreach { case (sink, source) => sink <> source }
    out <> arb.io.out
  }
}
