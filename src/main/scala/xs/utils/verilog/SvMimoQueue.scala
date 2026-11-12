/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xs.utils.verilog

import chisel3._
import chisel3.experimental.inlineInstance
import chisel3.util._

class SvMimoQueuePort(dw: Int = 32, enqNum: Int = 1, deqNum: Int = 1, depth: Int = 4) extends Bundle {
  val enq = new Bundle {
    val valid = Input(UInt(enqNum.W))
    val bits = Input(UInt((enqNum * dw).W))
    val ready = Output(UInt(enqNum.W))
  }
  val deq = new Bundle {
    val valid = Output(UInt(deqNum.W))
    val bits = Output(UInt((deqNum * dw).W))
    val ready = Input(UInt(deqNum.W))
  }
  val count = Output(UInt(log2Ceil(depth + 1).W))
}

class SvMimoQueueImpl(dw: Int = 32, enqNum: Int = 1, deqNum: Int = 1, depth: Int = 4, enqIsCompact: Boolean = false)
    extends VlogExtModule(Map(
      "DATA_WIDTH" -> dw,
      "ENQ_NUM" -> enqNum,
      "DEQ_NUM" -> deqNum,
      "DEPTH" -> depth,
      "ENQ_COMPACT" -> Seq(enqIsCompact).count(identity),
    )) {
  require(dw >= 1, "SvMimoQueue requires a known, positive data width")
  require(enqNum >= 1, "SvMimoQueue requires at least one enqueue lane")
  require(deqNum >= 1, "SvMimoQueue requires at least one dequeue lane")
  require(depth >= math.max(enqNum, deqNum), "SvMimoQueue DEPTH must be at least max(ENQ_NUM, DEQ_NUM)")

  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))
  val io = IO(new SvMimoQueuePort(dw = dw, enqNum = enqNum, deqNum = deqNum, depth = depth))

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvMimoQueueImpl)
  rtl.register(this, SvMimoQueueImpl)
}

object SvMimoQueueImpl extends VlogModule {
  override val moduleName = "SvMimoQueue"
  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter DATA_WIDTH = 32,
       |  parameter ENQ_NUM = 1,
       |  parameter DEQ_NUM = 1,
       |  parameter DEPTH = 4,
       |  parameter ENQ_COMPACT = 0
       |) (
       |  input  wire  [ENQ_NUM - 1:0]              io_enq_valid,
       |  input  wire  [ENQ_NUM * DATA_WIDTH - 1:0] io_enq_bits,
       |  output logic [ENQ_NUM - 1:0]              io_enq_ready,
       |  output logic [DEQ_NUM - 1:0]              io_deq_valid,
       |  output logic [DEQ_NUM * DATA_WIDTH - 1:0] io_deq_bits,
       |  input  wire  [DEQ_NUM - 1:0]              io_deq_ready,
       |  output wire  [$$clog2(DEPTH + 1) - 1:0]   io_count,
       |  input  wire                               clock,
       |  input  wire                               reset
       |);
       |  localparam integer ADDR_WIDTH = (DEPTH == 1) ? 1 : $$clog2(DEPTH);
       |  localparam integer COUNT_WIDTH = $$clog2(DEPTH + 1);
       |  localparam integer ENQ_COUNT_WIDTH = $$clog2(ENQ_NUM + 1);
       |  localparam integer DEQ_COUNT_WIDTH = $$clog2(DEQ_NUM + 1);
       |
       |  // The core uses an explicit active-high asynchronous reset by convention.
       |  // Payload memory and data registers are unreset.
       |  // Dequeue lanes form an ordered prefix window, not independent Decoupled queues.
       |  // io_deq_ready must be a low-lane prefix; remaining entries shift toward lane 0.
       |  logic [ADDR_WIDTH - 1:0] head;
       |  logic [COUNT_WIDTH - 1:0] count;
       |  logic [DATA_WIDTH - 1:0] mem [DEPTH];
       |
       |  // Chisel equivalent (base < DEPTH, offset <= DEPTH): val sum = base +& offset
       |  //   Mux(sum >= DEPTH.U, sum - DEPTH.U, sum)(ADDR_WIDTH - 1, 0)
       |  // The SV if/else also retains the raw sum when the comparison is unknown.
       |  function automatic [ADDR_WIDTH - 1:0] add_mod_depth(
       |    input logic [ADDR_WIDTH - 1:0] base,
       |    input logic [ADDR_WIDTH:0] offset
       |  );
       |    logic [ADDR_WIDTH:0] sum;
       |    logic [ADDR_WIDTH:0] wrapped_sum;
       |    sum = {1'b0, base} + offset;
       |    if (sum >= (ADDR_WIDTH + 1)'(DEPTH)) wrapped_sum = sum - (ADDR_WIDTH + 1)'(DEPTH);
       |    else wrapped_sum = sum;
       |    return wrapped_sum[ADDR_WIDTH - 1:0];
       |  endfunction
       |
       |  wire [ENQ_NUM - 1:0] enq_fire = io_enq_valid & io_enq_ready;
       |  wire [DEQ_NUM - 1:0] deq_fire = io_deq_valid & io_deq_ready;
       |  wire [ENQ_COUNT_WIDTH - 1:0] enq_fire_count;
       |  wire [DEQ_COUNT_WIDTH - 1:0] deq_fire_count;
       |  wire [ADDR_WIDTH:0] free_count = (ADDR_WIDTH + 1)'(DEPTH) - (ADDR_WIDTH + 1)'(count);
       |  wire [COUNT_WIDTH - 1:0] next_count = count + COUNT_WIDTH'(enq_fire_count) - COUNT_WIDTH'(deq_fire_count);
       |  wire [ADDR_WIDTH - 1:0] tail = add_mod_depth(head, (ADDR_WIDTH + 1)'(count));
       |  wire [ADDR_WIDTH - 1:0] next_head = add_mod_depth(head, (ADDR_WIDTH + 1)'(deq_fire_count));
       |  wire [ADDR_WIDTH:0] enq_rank [ENQ_NUM];
       |  logic [ADDR_WIDTH - 1:0] enq_addr [ENQ_NUM];
       |  logic [ENQ_NUM - 1:0] write_match [DEPTH];
       |  wire [DATA_WIDTH - 1:0] write_data [DEPTH];
       |  logic [ADDR_WIDTH - 1:0] next_addr [DEQ_NUM];
       |  logic [ENQ_NUM - 1:0] forward_match [DEQ_NUM];
       |  wire [DATA_WIDTH - 1:0] forward_data [DEQ_NUM];
       |
       |  assign io_count = count;
       |  assign enq_rank[0] = '0;
       |
       |  ${ctx.ref(SvPopcountImpl)} #(.INPUT_SIZE(ENQ_NUM)) u_enq_fire_count (
       |    .io_in(enq_fire), .io_out(enq_fire_count)
       |  );
       |  ${ctx.ref(SvPopcountImpl)} #(.INPUT_SIZE(DEQ_NUM)) u_deq_fire_count (
       |    .io_in(deq_fire), .io_out(deq_fire_count)
       |  );
       |
       |  generate
       |    if (ENQ_COMPACT != 0) begin : compact_rank
       |      // Chisel equivalent: enq_rank := VecInit((0 until ENQ_NUM).map(_.U((ADDR_WIDTH + 1).W)))
       |      for (genvar e = 1; e < ENQ_NUM; e++) begin : gen_rank_count
       |        assign enq_rank[e] = (ADDR_WIDTH + 1)'(e);
       |      end
       |    end else begin : sparse_rank
       |      // Chisel equivalent: enq_rank := VecInit((0 until ENQ_NUM).map(e =>
       |      //   PopCount(io_enq_valid.asBools.take(e)).pad(ADDR_WIDTH + 1)))
       |      for (genvar e = 1; e < ENQ_NUM; e++) begin : gen_rank_count
       |        assign enq_rank[e][ADDR_WIDTH:$$clog2(e + 1)] = '0;
       |        ${ctx.ref(SvPopcountImpl)} #(.INPUT_SIZE(e)) u_rank_count (
       |          .io_in(io_enq_valid[e - 1:0]),
       |          .io_out(enq_rank[e][$$clog2(e + 1) - 1:0])
       |        );
       |      end
       |    end
       |  endgenerate
       |
       |  // Chisel equivalent: io_enq_ready := VecInit(enq_rank.map(_ < free_count)).asUInt
       |  //   enq_addr := VecInit(enq_rank.map(r => ((tail +& r) % DEPTH.U)(ADDR_WIDTH - 1, 0)))
       |  always_comb begin
       |    for (integer e = 0; e < ENQ_NUM; e++) begin
       |      io_enq_ready[e] = enq_rank[e] < free_count;
       |      enq_addr[e] = add_mod_depth(tail, enq_rank[e]);
       |    end
       |  end
       |
       |  // Chisel equivalent: write_match := VecInit((0 until DEPTH).map(i =>
       |  //   VecInit((0 until ENQ_NUM).map(e => enq_fire(e) && enq_addr(e) === i.U)).asUInt))
       |  // Firing lanes have distinct ranks < free_count <= DEPTH, hence distinct addresses.
       |  always_comb begin
       |    for (integer i = 0; i < DEPTH; i++) begin
       |      for (integer e = 0; e < ENQ_NUM; e++) begin
       |        write_match[i][e] = enq_fire[e] && (enq_addr[e] == ADDR_WIDTH'(i));
       |      end
       |    end
       |  end
       |
       |  // Chisel equivalent: next_addr := VecInit((0 until DEQ_NUM).map(i =>
       |  //   ((next_head +& i.U) % DEPTH.U)(ADDR_WIDTH - 1, 0)))
       |  //   forward_match := VecInit(next_addr.map(a => VecInit(enq_addr.indices.map(e => enq_fire(e) && enq_addr(e) === a)).asUInt))
       |  always_comb begin
       |    for (integer i = 0; i < DEQ_NUM; i++) begin
       |      next_addr[i] = add_mod_depth(next_head, (ADDR_WIDTH + 1)'(i));
       |      for (integer e = 0; e < ENQ_NUM; e++) begin
       |        forward_match[i][e] = enq_fire[e] && (enq_addr[e] == next_addr[i]);
       |      end
       |    end
       |  end
       |
       |  // Chisel equivalent for each match row m: data := (0 until ENQ_NUM).map(e =>
       |  //   Mux(m(e), io_enq_bits((e + 1) * DATA_WIDTH - 1, e * DATA_WIDTH), 0.U(DATA_WIDTH.W))).reduce(_ | _)
       |  // Both mux banks return zero on a miss and OR payloads on multiple hits, even with ENQ_NUM == 1.
       |  generate
       |    for (genvar i = 0; i < DEPTH; i++) begin : gen_write_mux
       |      ${ctx.ref(SvMux1HImpl)} #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(ENQ_NUM)) u_data (
       |        .io_sel(write_match[i]), .io_in(io_enq_bits), .io_out(write_data[i])
       |      );
       |    end
       |    for (genvar i = 0; i < DEQ_NUM; i++) begin : gen_forward_mux
       |      ${ctx.ref(SvMux1HImpl)} #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(ENQ_NUM)) u_data (
       |        .io_sel(forward_match[i]), .io_in(io_enq_bits), .io_out(forward_data[i])
       |      );
       |    end
       |  endgenerate
       |
       |  // Chisel equivalent: mem.indices.foreach(i => when(write_match(i).orR) { mem(i) := write_data(i) })
       |  // mem is an unreset Reg(Vec(DEPTH, UInt(DATA_WIDTH.W))); writes occur on the rising edge.
       |  always_ff @(posedge clock) begin
       |    for (integer i = 0; i < DEPTH; i++) begin
       |      if (|write_match[i]) mem[i] <= write_data[i];
       |    end
       |  end
       |
       |  // Chisel equivalent (clock, asynchronous reset): head/count/io_deq_valid are RegInit(0.U).
       |  //   head := next_head; count := next_count; io_deq_valid := VecInit((0 until DEQ_NUM).map(_.U < next_count)).asUInt
       |  //   when(!reset.asBool && i.U < next_count) { bits(i) := Mux(forward_match(i).orR, forward_data(i), mem(next_addr(i))) }
       |  // bits(i) denotes the DATA_WIDTH-bit output lane: unreset, held while invalid or in reset.
       |  always_ff @(posedge clock or posedge reset) begin
       |    if (reset) begin
       |      head <= '0;
       |      count <= '0;
       |      io_deq_valid <= '0;
       |    end else begin
       |      head <= next_head;
       |      count <= next_count;
       |      // Forward same-cycle writes into the next registered output window.
       |      for (integer i = 0; i < DEQ_NUM; i++) begin
       |        io_deq_valid[i] <= i < next_count;
       |        if (i < next_count) begin
       |          io_deq_bits[i * DATA_WIDTH +: DATA_WIDTH] <= (|forward_match[i]) ? forward_data[i] : mem[next_addr[i]];
       |        end
       |      end
       |    end
       |  end
       |endmodule""".stripMargin
}

/**
 * Multi-input/multi-output queue backed by the single registered-output SvMimoQueueImpl RTL core.
 *
 * Reset convention: the RTL core uses active-high asynchronous reset, matching the
 * repository's SvFastQueue SV convention. This intentionally changes the old wrapper's
 * Chisel-inferred reset behavior.
 *
 * Dequeue contract: io.deq is a packed, ordered prefix-transfer window rooted at the
 * queue head. io.deq.ready must be a low-lane prefix. If only lower lanes fire, the
 * remaining head entries shift toward lane 0 in the next registered output window;
 * these lanes are not independent Decoupled queues with per-lane stability semantics.
 */
class SvMimoQueue[T <: Data]
(
  gen: T = UInt(32.W),
  enqNum: Int = 1,
  deqNum: Int = 1,
  size: Int = 4,
  enqIsCompact: Boolean = false
) extends Module {
  private val dataWidth = gen.getWidth
  require(dataWidth > 0, "SvMimoQueue requires a known, positive payload width")
  require(enqNum >= 1, "SvMimoQueue requires at least one enqueue lane")
  require(deqNum >= 1, "SvMimoQueue requires at least one dequeue lane")
  require(size >= math.max(enqNum, deqNum), "SvMimoQueue size must be at least max(enqNum, deqNum)")

  val io = IO(new Bundle {
    val enq = Vec(enqNum, Flipped(Decoupled(gen)))
    val deq = Vec(deqNum, Decoupled(gen))
    val count = Output(UInt(log2Ceil(size + 1).W))
  })

  val q = Module(new SvMimoQueueImpl(
    dw = dataWidth,
    enqNum = enqNum,
    deqNum = deqNum,
    depth = size,
    enqIsCompact = enqIsCompact
  ))

  q.io.enq.valid := VecInit(io.enq.map(_.valid)).asUInt
  q.io.enq.bits := VecInit(io.enq.map(_.bits.asUInt)).asUInt
  io.enq.zip(q.io.enq.ready.asBools).foreach { case (enq, ready) =>
    enq.ready := ready
  }

  private val deqBits = q.io.deq.bits.asTypeOf(Vec(deqNum, UInt(dataWidth.W)))
  q.io.deq.valid.asBools.zip(io.deq).foreach { case (valid, deq) =>
    deq.valid := valid
  }
  deqBits.zip(io.deq).foreach { case (bits, deq) =>
    deq.bits := bits.asTypeOf(gen)
  }
  q.io.deq.ready := VecInit(io.deq.map(_.ready)).asUInt
  io.count := q.io.count

  q.clock := clock
  q.reset := reset.asAsyncReset

  private val enqValidNum = PopCount(io.enq.map(_.valid))
  if (enqIsCompact) {
    for (i <- 0 until enqNum) {
      assert(Mux(i.U < enqValidNum, io.enq(i).valid, !io.enq(i).valid),
        "compact SvMimoQueue enqueue valids must be a prefix")
    }
  }

  private val deqReadyNum = PopCount(io.deq.map(_.ready))
  for (i <- 0 until deqNum) {
    assert(Mux(i.U < deqReadyNum, io.deq(i).ready, !io.deq(i).ready),
      "SvMimoQueue dequeue readies must be a prefix")
  }

  inlineInstance(this)
}
