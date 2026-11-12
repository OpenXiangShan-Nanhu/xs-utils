package xs.utils.verilog

import chisel3._
import chisel3.experimental.inlineInstance
import chisel3.util._

class SvFastQueuePort(dw:Int = 32, depth:Int = 2) extends Bundle {
  val enq       = Flipped(Decoupled(UInt(dw.W)))
  val deq       = Decoupled(UInt(dw.W))
  val waterline = Output(UInt((depth + 1).W))
  val count     = Output(UInt(log2Ceil(depth + 1).W))
  val free      = Output(UInt(log2Ceil(depth + 1).W))
}

class SvFastQueueBaseImpl(dw:Int, depth:Int, pm: Map[String, Param]) extends VlogExtModule(pm) {
  require(dw >= 1, "SvFastQueue requires a known, positive data width")
  require(depth >= 1, "SvFastQueue requires a positive depth")
  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))
  val io = IO(new SvFastQueuePort(dw, depth))
}

class SvFastQueueD1Impl(dw:Int = 32, pipe:Boolean = true, flow:Boolean = false, cleanOut:Boolean = false) extends SvFastQueueBaseImpl(dw, 1, Map(
  "DATA_WIDTH" -> dw,
  "PIPE" -> Seq(pipe).count(identity),
  "FLOW" -> Seq(flow).count(identity),
  "CLEAN_OUT" -> Seq(cleanOut).count(identity),
)) {
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvFastQueueD1Impl)
  rtl.register(this, SvFastQueueD1Impl)
}

object SvFastQueueD1Impl extends VlogModule {
  override val moduleName = "SvFastQueueD1"
  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter DATA_WIDTH = 32,
       |  parameter PIPE = 1,
       |  parameter FLOW = 0,
       |  parameter CLEAN_OUT = 0
       |) (
       |  input  wire                     io_enq_valid,
       |  input  wire  [DATA_WIDTH - 1:0] io_enq_bits,
       |  output wire                     io_enq_ready,
       |  output wire                     io_deq_valid,
       |  output wire  [DATA_WIDTH - 1:0] io_deq_bits,
       |  input  wire                     io_deq_ready,
       |  output logic [1:0]              io_waterline,
       |  output wire                     io_count,
       |  output wire                     io_free,
       |  input  wire                     clock,
       |  input  wire                     reset
       |);
       |  logic outv;
       |  logic [DATA_WIDTH - 1 : 0] outd;
       |
       |  wire enq_en;
       |  wire enq_fire = io_enq_valid && enq_en;
       |  wire deq_fire = io_deq_valid && io_deq_ready;
       |  wire bypass;
       |  wire [DATA_WIDTH - 1 : 0] deq_bits;
       |
       |  generate
       |    if(PIPE == 1) begin : pipe_enabled
       |      assign enq_en = (outv && io_deq_ready) || !outv;
       |    end else begin : pipe_disabled
       |      assign enq_en = !outv;
       |    end
       |
       |    if(CLEAN_OUT == 1) begin : clean_out_enabled
       |      assign deq_bits = outv ? outd : {DATA_WIDTH{1'b0}};
       |    end else begin : clean_out_disabled
       |      assign deq_bits = outd;
       |    end
       |
       |    if(FLOW == 1) begin : flow_enabled
       |      assign bypass = !outv && enq_fire && io_deq_ready;
       |      assign io_deq_valid = outv || io_enq_valid;
       |      assign io_deq_bits = !outv ? io_enq_bits : deq_bits;
       |    end else begin : flow_disabled
       |      assign bypass = 1'b0;
       |      assign io_deq_valid = outv;
       |      assign io_deq_bits = deq_bits;
       |    end
       |  endgenerate
       |
       |  assign io_enq_ready = enq_en;
       |  // Chisel equivalent (clock, asynchronous reset): val io_waterline = RegInit(1.U(2.W))
       |  //   when(enq_fire && !deq_fire) { io_waterline := 2.U }.elsewhen(deq_fire && !enq_fire) { io_waterline := 1.U }
       |  always_ff @ (posedge clock or posedge reset) begin
       |    if(reset) begin
       |      io_waterline <= 2'b01;
       |    end else if(enq_fire && !deq_fire) begin
       |      io_waterline <= 2'b10;
       |    end else if(deq_fire && !enq_fire) begin
       |      io_waterline <= 2'b01;
       |    end
       |  end
       |
       |  // Chisel equivalent: io_count := OHToUInt(io_waterline); io_free := OHToUInt(Reverse(io_waterline))
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(2)) u_count (
       |    .io_in(io_waterline), .io_out(io_count)
       |  );
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(2)) u_free (
       |    .io_in({io_waterline[0], io_waterline[1]}), .io_out(io_free)
       |  );
       |
       |  always_ff @ (posedge clock or posedge reset) begin
       |    if(reset) begin
       |      outv <= 1'b0;
       |    end else begin
       |      outv <= (outv && !deq_fire) || (enq_fire && !bypass);
       |    end
       |  end
       |
       |  always_ff @ (posedge clock) begin
       |    if(enq_fire && !bypass) begin
       |      outd <= io_enq_bits;
       |    end
       |  end
       |
       |endmodule""".stripMargin
}

class SvFastQueueD2Impl(dw:Int = 32, flow:Boolean = false, cleanOut:Boolean = false) extends SvFastQueueBaseImpl(dw, 2, Map(
  "DATA_WIDTH" -> dw,
  "FLOW" -> Seq(flow).count(identity),
  "CLEAN_OUT" -> Seq(cleanOut).count(identity),
)) {
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvFastQueueD2Impl)
  rtl.register(this, SvFastQueueD2Impl)
}

object SvFastQueueD2Impl extends VlogModule {
  override val moduleName = "SvFastQueueD2"
  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter DATA_WIDTH = 32,
       |  parameter FLOW = 0,
       |  parameter CLEAN_OUT = 0,
       |  parameter PIPE = 0
       |) (
       |  input  wire                     io_enq_valid,
       |  input  wire  [DATA_WIDTH - 1:0] io_enq_bits,
       |  output wire                     io_enq_ready,
       |  output wire                     io_deq_valid,
       |  output wire  [DATA_WIDTH - 1:0] io_deq_bits,
       |  input  wire                     io_deq_ready,
       |  output logic [2:0]              io_waterline,
       |  output wire  [1:0]              io_count,
       |  output wire  [1:0]              io_free,
       |  input  wire                     clock,
       |  input  wire                     reset
       |);
       |  logic enqv;
       |  logic deqv;
       |  logic [DATA_WIDTH - 1 : 0] enqd;
       |  logic [DATA_WIDTH - 1 : 0] deqd;
       |
       |  wire enq_fire = io_enq_valid && io_enq_ready;
       |  wire deq_fire = io_deq_valid && io_deq_ready;
       |  wire bypass;
       |  wire [DATA_WIDTH - 1 : 0] deq_bits;
       |  wire store = enq_fire && !bypass;
       |  wire input_to_enq = store && deqv && (!deq_fire || enqv);
       |  wire deqd_write = (deq_fire && enqv) || (store && (!deqv || deq_fire));
       |
       |  generate
       |    if(PIPE == 1) begin : pipe_enabled
       |      assign io_enq_ready = !enqv || io_deq_ready;
       |    end else begin : pipe_disabled
       |      assign io_enq_ready = !enqv;
       |    end
       |
       |    if(CLEAN_OUT == 1) begin : clean_out_enabled
       |      assign deq_bits = deqv ? deqd : {DATA_WIDTH{1'b0}};
       |    end else begin : clean_out_disabled
       |      assign deq_bits = deqd;
       |    end
       |
       |    if(FLOW == 1) begin : flow_enabled
       |      assign bypass = !deqv && enq_fire && io_deq_ready;
       |      assign io_deq_valid = deqv || io_enq_valid;
       |      assign io_deq_bits = !deqv ? io_enq_bits : deq_bits;
       |    end else begin : flow_disabled
       |      assign bypass = 1'b0;
       |      assign io_deq_valid = deqv;
       |      assign io_deq_bits = deq_bits;
       |    end
       |  endgenerate
       |
       |  // Chisel equivalent (clock, asynchronous reset): val io_waterline = RegInit(1.U(3.W))
       |  //   when(enq_fire && !deq_fire) { io_waterline := (io_waterline << 1)(2, 0) }
       |  //     .elsewhen(deq_fire && !enq_fire) { io_waterline := io_waterline >> 1 }
       |  always_ff @ (posedge clock or posedge reset) begin
       |    if(reset) begin
       |      io_waterline <= 3'b001;
       |    end else if(enq_fire && !deq_fire) begin
       |      io_waterline <= {io_waterline[1 : 0], 1'b0};
       |    end else if(deq_fire && !enq_fire) begin
       |      io_waterline <= {1'b0, io_waterline[2 : 1]};
       |    end
       |  end
       |
       |  // Chisel equivalent: io_count := OHToUInt(io_waterline); io_free := OHToUInt(Reverse(io_waterline))
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(3)) u_count (
       |    .io_in(io_waterline), .io_out(io_count)
       |  );
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(3)) u_free (
       |    .io_in({io_waterline[0], io_waterline[1], io_waterline[2]}), .io_out(io_free)
       |  );
       |
       |  // Chisel equivalent (clock, asynchronous reset): val Seq(enqv, deqv) = Seq.fill(2)(RegInit(false.B))
       |  //   enqv := (enqv && !deq_fire) || input_to_enq; deqv := (deqv && !deq_fire) || enqv || store
       |  always_ff @ (posedge clock or posedge reset) begin
       |    if(reset) begin
       |      enqv <= 1'b0;
       |      deqv <= 1'b0;
       |    end else begin
       |      enqv <= (enqv && !deq_fire) || input_to_enq;
       |      deqv <= (deqv && !deq_fire) || enqv || store;
       |    end
       |  end
       |
       |  // Chisel equivalent (unreset data registers, known controls): when(deqd_write) { deqd := Mux(deq_fire && enqv, enqd, io_enq_bits) }
       |  //   when(input_to_enq) { enqd := io_enq_bits }
       |  always_ff @ (posedge clock) begin
       |    if(deqd_write) begin
       |      if(deq_fire && enqv) begin
       |        deqd <= enqd;
       |      end else begin
       |        deqd <= io_enq_bits;
       |      end
       |    end
       |    if(input_to_enq) begin
       |      enqd <= io_enq_bits;
       |    end
       |  end
       |endmodule""".stripMargin
}

class SvFastQueueD3Impl(dw:Int = 32, flow:Boolean = false, cleanOut:Boolean = false) extends SvFastQueueBaseImpl(dw, 3, Map(
  "DATA_WIDTH" -> dw,
  "FLOW" -> Seq(flow).count(identity),
  "CLEAN_OUT" -> Seq(cleanOut).count(identity),
)) {
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvFastQueueD3Impl)
  rtl.register(this, SvFastQueueD3Impl)
}

object SvFastQueueD3Impl extends VlogModule {
  override val moduleName = "SvFastQueueD3"
  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter DATA_WIDTH = 32,
       |  parameter FLOW = 0,
       |  parameter CLEAN_OUT = 0
       |) (
       |  input  wire                     io_enq_valid,
       |  input  wire  [DATA_WIDTH - 1:0] io_enq_bits,
       |  output wire                     io_enq_ready,
       |  output wire                     io_deq_valid,
       |  output wire  [DATA_WIDTH - 1:0] io_deq_bits,
       |  input  wire                     io_deq_ready,
       |  output logic [3:0]              io_waterline,
       |  output wire  [1:0]              io_count,
       |  output wire  [1:0]              io_free,
       |  input  wire                     clock,
       |  input  wire                     reset
       |);
       |  logic enqv, midv, deqv;
       |  logic [DATA_WIDTH - 1 : 0] enqd, midd, deqd;
       |  wire store;
       |  wire empty = io_waterline[0];
       |  wire full = io_waterline[3];
       |  wire enq_fire = io_enq_valid && io_enq_ready;
       |  wire deq_fire = io_deq_valid && io_deq_ready;
       |  wire mid_to_deq = deq_fire && midv;
       |  wire enq_to_deq = deq_fire && !midv && enqv;
       |  wire enq_to_mid = enqv && !enq_to_deq && (!midv || mid_to_deq);
       |  wire input_to_deq = store && (!deqv || (deq_fire && !midv && !enqv));
       |  wire input_to_enq = store && !input_to_deq;
       |  wire [DATA_WIDTH - 1 : 0] deq_bits;
       |
       |  generate
       |    if(CLEAN_OUT == 1) begin : clean_out_enabled
       |      assign deq_bits = deqv ? deqd : {DATA_WIDTH{1'b0}};
       |    end else begin : clean_out_disabled
       |      assign deq_bits = deqd;
       |    end
       |    if(FLOW == 1) begin : flow_enabled
       |      assign store = enq_fire && !(empty && io_deq_ready);
       |      assign io_deq_valid = deqv || io_enq_valid;
       |      assign io_deq_bits = !deqv ? io_enq_bits : deq_bits;
       |    end else begin : flow_disabled
       |      assign store = enq_fire;
       |      assign io_deq_valid = deqv;
       |      assign io_deq_bits = deq_bits;
       |    end
       |  endgenerate
       |
       |  assign io_enq_ready = !full;
       |  // Chisel-style structure (clock, asynchronous reset): val Seq(enqv, midv, deqv) = Seq.fill(3)(RegInit(false.B))
       |  //   val io_waterline = RegInit(1.U(4.W)); flags update independently, allowing concurrent moves and refills.
       |  always_ff @ (posedge clock or posedge reset) begin
       |    if(reset) begin
       |      enqv <= 1'b0;
       |      midv <= 1'b0;
       |      deqv <= 1'b0;
       |      io_waterline <= 4'b0001;
       |    end else begin
       |      enqv <= (enqv && !enq_to_deq && !enq_to_mid) || input_to_enq;
       |      midv <= (midv && !mid_to_deq) || enq_to_mid;
       |      deqv <= (deqv && !deq_fire) || mid_to_deq || enq_to_deq || input_to_deq;
       |      // Chisel equivalent: when(enq_fire && !deq_fire) { io_waterline := (io_waterline << 1)(3, 0) }
       |      //   .elsewhen(deq_fire && !enq_fire) { io_waterline := io_waterline >> 1 } // otherwise hold
       |      if(enq_fire && !deq_fire) begin
       |        io_waterline <= {io_waterline[2 : 0], 1'b0};
       |      end else if(deq_fire && !enq_fire) begin
       |        io_waterline <= {1'b0, io_waterline[3 : 1]};
       |      end
       |    end
       |  end
       |
       |  // Chisel equivalent: io_count := OHToUInt(io_waterline); io_free := OHToUInt(Reverse(io_waterline))
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(4)) u_count (
       |    .io_in(io_waterline), .io_out(io_count)
       |  );
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(4)) u_free (
       |    .io_in({io_waterline[0], io_waterline[1], io_waterline[2], io_waterline[3]}), .io_out(io_free)
       |  );
       |
       |  // Chisel equivalent (unreset data registers): when(deq_fire || input_to_deq) {
       |  //   when(mid_to_deq) { deqd := midd }.elsewhen(enq_to_deq) { deqd := enqd }.elsewhen(input_to_deq) { deqd := io_enq_bits } }
       |  //   when(input_to_enq) { enqd := io_enq_bits }; when(enq_to_mid) { midd := enqd }
       |  always_ff @ (posedge clock) begin
       |    if(deq_fire || input_to_deq) begin
       |      if(mid_to_deq) begin
       |        deqd <= midd;
       |      end else if(enq_to_deq) begin
       |        deqd <= enqd;
       |      end else if(input_to_deq) begin
       |        deqd <= io_enq_bits;
       |      end
       |    end
       |    if(input_to_enq) enqd <= io_enq_bits;
       |    if(enq_to_mid) midd <= enqd;
       |  end
       |endmodule""".stripMargin
}

class SvFastQueueDNImpl(dw:Int = 32, depth:Int = 4, flow:Boolean = false, cleanOut:Boolean = false) extends SvFastQueueBaseImpl(dw, depth, Map(
  "DATA_WIDTH" -> dw,
  "DEPTH" -> depth,
  "FLOW" -> Seq(flow).count(identity),
  "CLEAN_OUT" -> Seq(cleanOut).count(identity),
)) {
  require(dw >= 1)
  require(depth >= 4)

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvFastQueueDNImpl)
  rtl.register(this, SvFastQueueDNImpl)
}

object SvFastQueueDNImpl extends VlogModule {
  override val moduleName = "SvFastQueueDN"
  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter DATA_WIDTH = 32,
       |  parameter DEPTH = 4,
       |  parameter FLOW = 0,
       |  parameter CLEAN_OUT = 0
       |) (
       |  input  wire                             io_enq_valid,
       |  input  wire  [DATA_WIDTH - 1:0]         io_enq_bits,
       |  output wire                             io_enq_ready,
       |  output wire                             io_deq_valid,
       |  output wire  [DATA_WIDTH - 1:0]         io_deq_bits,
       |  input  wire                             io_deq_ready,
       |  input  wire                             clock,
       |  input  wire                             reset,
       |  output logic [DEPTH:0]                  io_waterline,
       |  output wire  [$$clog2(DEPTH + 1) - 1:0] io_count,
       |  output wire  [$$clog2(DEPTH + 1) - 1:0] io_free
       |);
       |  localparam integer MEM_DEPTH = DEPTH - 2;
       |
       |  logic enqv, deqv;
       |  logic [DATA_WIDTH - 1 : 0] enqd, deqd;
       |  logic [DATA_WIDTH - 1 : 0] mem [MEM_DEPTH - 1 : 0];
       |  logic [MEM_DEPTH - 1 : 0] mem_wr_ptr_v, mem_rd_ptr_v;
       |  logic mem_wr_ptr_f, mem_rd_ptr_f;
       |  logic [MEM_DEPTH * DATA_WIDTH - 1:0] mem_read_inputs;
       |  logic [DEPTH:0] reversed_waterline;
       |  wire store;
       |  wire empty = io_waterline[0];
       |  wire full = io_waterline[DEPTH];
       |  wire mem_ptr_equal = mem_wr_ptr_v == mem_rd_ptr_v;
       |  wire mem_empty = mem_ptr_equal && mem_wr_ptr_f == mem_rd_ptr_f;
       |  wire mem_full = mem_ptr_equal && mem_wr_ptr_f != mem_rd_ptr_f;
       |  wire enq_fire = io_enq_valid && io_enq_ready;
       |  wire deq_fire = io_deq_valid && io_deq_ready;
       |  wire mem_rd_ptr_mv = deq_fire && !mem_empty;
       |  wire enq_to_deq = deq_fire && mem_empty && enqv;
       |  wire mem_wr_ptr_mv = enqv && !enq_to_deq && (!mem_full || mem_rd_ptr_mv);
       |  wire input_to_deq = store && (!deqv || (deq_fire && mem_empty && !enqv));
       |  wire input_to_enq = store && !input_to_deq;
       |  wire deqd_write = mem_rd_ptr_mv || enq_to_deq || input_to_deq;
       |  wire [DATA_WIDTH - 1 : 0] deq_bits;
       |  wire [DATA_WIDTH - 1:0] mem_read_data;
       |
       |  generate
       |    if(CLEAN_OUT == 1) begin : clean_out_enabled
       |      assign deq_bits = deqv ? deqd : {DATA_WIDTH{1'b0}};
       |    end else begin : clean_out_disabled
       |      assign deq_bits = deqd;
       |    end
       |
       |    if(FLOW == 1) begin : flow_enabled
       |      assign store = enq_fire && !(empty && enq_fire && io_deq_ready);
       |      assign io_deq_valid = deqv || io_enq_valid;
       |      assign io_deq_bits = !deqv ? io_enq_bits : deq_bits;
       |    end else begin : flow_disabled
       |      assign store = enq_fire;
       |      assign io_deq_valid = deqv;
       |      assign io_deq_bits = deq_bits;
       |    end
       |  endgenerate
       |
       |  // Chisel equivalent: mem_read_inputs := VecInit(mem).asUInt // lane zero occupies the low bits
       |  always_comb begin
       |    for(integer i = 0; i < MEM_DEPTH; i = i + 1) begin
       |      mem_read_inputs[i * DATA_WIDTH +: DATA_WIDTH] = mem[i];
       |    end
       |  end
       |  // Chisel equivalent: mem_read_data := Mux1H(mem_rd_ptr_v, mem) // MEM_DEPTH >= 2; read pointer is one-hot
       |  ${ctx.ref(SvMux1HImpl)} #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(MEM_DEPTH)) u_mem_read (
       |    .io_sel(mem_rd_ptr_v), .io_in(mem_read_inputs), .io_out(mem_read_data)
       |  );
       |
       |  assign io_enq_ready = !full;
       |  // Chisel-style structure (clock, asynchronous reset): enqv/deqv and wrap flags are RegInit(false.B).
       |  //   io_waterline = RegInit(1.U((DEPTH + 1).W)); mem_wr_ptr_v/mem_rd_ptr_v = RegInit(1.U(MEM_DEPTH.W)).
       |  // Independent flags and pointers permit concurrent enqueue, dequeue and memory movement.
       |  always_ff @ (posedge clock or posedge reset) begin
       |    if(reset) begin
       |      enqv <= 1'b0;
       |      deqv <= 1'b0;
       |      io_waterline <= {{DEPTH{1'b0}}, 1'b1};
       |      mem_wr_ptr_v <= {{(MEM_DEPTH - 1){1'b0}}, 1'b1};
       |      mem_rd_ptr_v <= {{(MEM_DEPTH - 1){1'b0}}, 1'b1};
       |      mem_wr_ptr_f <= 1'b0;
       |      mem_rd_ptr_f <= 1'b0;
       |    end else begin
       |      enqv <= (enqv && !enq_to_deq && !mem_wr_ptr_mv) || input_to_enq;
       |      deqv <= (deqv && !deq_fire) || mem_rd_ptr_mv || enq_to_deq || input_to_deq;
       |
       |      // Chisel equivalent: when(enq_fire && !deq_fire) { io_waterline := (io_waterline << 1)(DEPTH, 0) }
       |      //   .elsewhen(deq_fire && !enq_fire) { io_waterline := io_waterline >> 1 } // otherwise hold
       |      if(enq_fire && !deq_fire) begin
       |        io_waterline <= {io_waterline[DEPTH - 1:0], 1'b0};
       |      end else if(deq_fire && !enq_fire) begin
       |        io_waterline <= {1'b0, io_waterline[DEPTH:1]};
       |      end
       |      // Chisel equivalent (each enabled pointer p and wrap flag f): p := Cat(p(MEM_DEPTH - 2, 0), p(MEM_DEPTH - 1))
       |      //   f := f ^ p(MEM_DEPTH - 1); disabled pointers/flags hold, RHS uses pre-edge values.
       |      if(mem_wr_ptr_mv) begin
       |        mem_wr_ptr_v <= {mem_wr_ptr_v[MEM_DEPTH - 2:0], mem_wr_ptr_v[MEM_DEPTH - 1]};
       |        mem_wr_ptr_f <= mem_wr_ptr_f ^ mem_wr_ptr_v[MEM_DEPTH - 1];
       |      end
       |      if(mem_rd_ptr_mv) begin
       |        mem_rd_ptr_v <= {mem_rd_ptr_v[MEM_DEPTH - 2:0], mem_rd_ptr_v[MEM_DEPTH - 1]};
       |        mem_rd_ptr_f <= mem_rd_ptr_f ^ mem_rd_ptr_v[MEM_DEPTH - 1];
       |      end
       |    end
       |  end
       |
       |  // Chisel equivalent: reversed_waterline := Reverse(io_waterline)
       |  always_comb begin
       |    for(integer n = 0; n <= DEPTH; n = n + 1) begin
       |      reversed_waterline[n] = io_waterline[DEPTH - n];
       |    end
       |  end
       |  // Chisel equivalent: io_count := OHToUInt(io_waterline); io_free := OHToUInt(Reverse(io_waterline))
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(DEPTH + 1)) u_count (
       |    .io_in(io_waterline), .io_out(io_count)
       |  );
       |  ${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(DEPTH + 1)) u_free (
       |    .io_in(reversed_waterline), .io_out(io_free)
       |  );
       |
       |  // Chisel equivalent (unreset data registers, known controls): when(deqd_write) {
       |  //   deqd := Mux(mem_rd_ptr_mv, mem_read_data, Mux(enq_to_deq, enqd, io_enq_bits)) }; when(input_to_enq) { enqd := io_enq_bits }
       |  //   mem.indices.foreach(i => when(mem_wr_ptr_mv && mem_wr_ptr_v(i)) { mem(i) := enqd }) // read before write
       |  always_ff @ (posedge clock) begin
       |    if(deqd_write) begin
       |      if(mem_rd_ptr_mv) deqd <= mem_read_data;
       |      else if(enq_to_deq) deqd <= enqd;
       |      else deqd <= io_enq_bits;
       |    end
       |    if(input_to_enq) enqd <= io_enq_bits;
       |    for(integer i = 0; i < MEM_DEPTH; i = i + 1) begin
       |      if(mem_wr_ptr_mv && mem_wr_ptr_v[i]) mem[i] <= enqd;
       |    end
       |  end
       |endmodule""".stripMargin
}

class SvFastQueue[T <: Data](gen: T = UInt(32.W), size: Int = 2, pipe:Boolean = true, flow: Boolean = false, noX:Boolean = false) extends Module {
  val io = IO(new Bundle {
    val enq       = Flipped(Decoupled(gen))
    val deq       = Decoupled(gen)
    val waterline = Output(UInt((size + 1).W))
    val count     = Output(UInt(log2Ceil(size + 1).W))
    val free      = Output(UInt(log2Ceil(size + 1).W))

    def freeNum = free
  })

  val q = size match {
    case 1 => Module(new SvFastQueueD1Impl(gen.getWidth, pipe, flow, noX))
    case 2 => Module(new SvFastQueueD2Impl(gen.getWidth, flow, noX))
    case 3 => Module(new SvFastQueueD3Impl(gen.getWidth, flow, noX))
    case _ => Module(new SvFastQueueDNImpl(gen.getWidth, size, flow, noX))
  }
  q.io.enq.valid := io.enq.valid
  q.io.enq.bits := io.enq.bits.asUInt
  io.enq.ready := q.io.enq.ready

  io.deq.valid := q.io.deq.valid
  io.deq.bits := q.io.deq.bits.asTypeOf(gen)
  q.io.deq.ready := io.deq.ready

  io.waterline := q.io.waterline
  io.count := q.io.count
  io.free := q.io.free

  q.clock := clock
  q.reset := reset.asAsyncReset

  inlineInstance(this)
}

object SvFastQueueRaw {
  def apply[T <: Data](enq: DecoupledIO[T], size: Int = 2, noX:Boolean = false, name:Option[String] = None): DecoupledIO[T] = {
    val dw = enq.bits.getWidth
    require(size >= 1)
    require(dw >= 1)
    val q = Module(new SvFastQueue(enq.bits.cloneType, size, true, false, noX))
    name.foreach(n => q.suggestName(n))
    q.io.enq <> enq
    q.io.deq
  }
}

class SvFastQueueFactory(noX:Boolean = false) {
  def apply[T <: Data](enq: DecoupledIO[T], size: Int, name:Option[String]): DecoupledIO[T] = SvFastQueueRaw(enq, size, noX, name)

  def apply[T <: Data](enq: DecoupledIO[T], name:Option[String]): DecoupledIO[T] = apply(enq, 2, name)

  def apply[T <: Data](enq: DecoupledIO[T], size: Int): DecoupledIO[T] = apply(enq, size, None)

  def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T] = apply(enq, 2, None)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], size: Int, name:Option[String]):Unit = deq <> apply(enq, size, name)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], size: Int):Unit = deq <> apply(enq, size, None)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], name:Option[String]):Unit = deq <> apply(enq, 2, name)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T]):Unit = deq <> apply(enq, 2, None)

  def apply[T <: Data](enq: DecoupledIO[T], enable: Boolean, size: Int): DecoupledIO[T] = if(enable) apply(enq, size, None) else enq

  def apply[T <: Data](enq: DecoupledIO[T], enable: Boolean): DecoupledIO[T] = apply(enq, enable, 2)
}

object SvFastQueueNoX extends SvFastQueueFactory(true)

object SvFastQueue extends SvFastQueueFactory(false)
