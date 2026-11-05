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
  val reset = IO(Input(Reset()))
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
       |  input  logic [ENQ_NUM - 1:0] io_enq_valid,
       |  input  logic [ENQ_NUM * DATA_WIDTH - 1:0] io_enq_bits,
       |  output logic [ENQ_NUM - 1:0] io_enq_ready,
       |  output logic [DEQ_NUM - 1:0] io_deq_valid,
       |  output logic [DEQ_NUM * DATA_WIDTH - 1:0] io_deq_bits,
       |  input  logic [DEQ_NUM - 1:0] io_deq_ready,
       |  output wire [$$clog2(DEPTH + 1) - 1:0] io_count,
       |  input  logic clock,
       |  input  logic reset
       |);
       |  localparam integer ADDR_WIDTH = (DEPTH == 1) ? 1 : $$clog2(DEPTH);
       |  localparam integer COUNT_WIDTH = $$clog2(DEPTH + 1);
       |  localparam integer ENQ_COUNT_WIDTH = $$clog2(ENQ_NUM + 1);
       |  localparam integer DEQ_COUNT_WIDTH = $$clog2(DEQ_NUM + 1);
       |
       |  // The core uses an explicit active-high asynchronous reset by convention.
       |  // This is a deliberate change from the old Chisel-inferred reset behavior.
       |  //
       |  // io_deq_* is a packed, ordered prefix-transfer window rooted at the queue head.
       |  // io_deq_ready must be a low-lane prefix. A partial dequeue shifts the remaining
       |  // head entries toward lane 0 on the next registered output window; these lanes are
       |  // not independent Decoupled queues with per-lane stability semantics.
       |  // Unpacked port views and transfer nets.
       |  logic enq_valid [ENQ_NUM];
       |  logic [DATA_WIDTH - 1:0] enq_bits [ENQ_NUM];
       |  logic [ENQ_NUM * DATA_WIDTH - 1:0] enq_mux_data;
       |  logic enq_ready [ENQ_NUM];
       |  logic deq_valid [DEQ_NUM];
       |  logic [DATA_WIDTH - 1:0] deq_bits [DEQ_NUM];
       |  logic deq_ready [DEQ_NUM];
       |  logic enq_fire [ENQ_NUM];
       |  logic deq_fire [DEQ_NUM];
       |
       |  // Registered queue state and payload memory.
       |  logic [ADDR_WIDTH - 1:0] head;
       |  logic [COUNT_WIDTH - 1:0] count;
       |  logic [DATA_WIDTH - 1:0] mem [DEPTH];
       |
       |  function automatic [ADDR_WIDTH - 1:0] add_mod_depth(
       |    input logic [ADDR_WIDTH - 1:0] base,
       |    input logic [ADDR_WIDTH:0] offset
       |  );
       |    logic [ADDR_WIDTH:0] sum;
       |    logic [ADDR_WIDTH:0] reduced_sum;
       |    begin
       |      sum = {1'b0, base} + offset;
       |      if (sum >= DEPTH) begin
       |        reduced_sum = sum - DEPTH;
       |        add_mod_depth = reduced_sum[ADDR_WIDTH - 1:0];
       |      end else begin
       |        add_mod_depth = sum[ADDR_WIDTH - 1:0];
       |      end
       |    end
       |  endfunction
       |
       |  wire [ADDR_WIDTH:0] free_count;
       |  wire [ADDR_WIDTH:0] count_offset;
       |  wire [ADDR_WIDTH:0] deq_fire_offset;
       |  wire [COUNT_WIDTH:0] enq_fire_count;
       |  wire [COUNT_WIDTH:0] deq_fire_count;
       |  wire [COUNT_WIDTH:0] next_count_ext;
       |  wire [COUNT_WIDTH - 1:0] next_count;
       |  wire [ADDR_WIDTH - 1:0] tail;
       |  wire [ADDR_WIDTH - 1:0] next_head;
       |
       |  assign io_count = count;
       |  assign free_count = DEPTH - count;
       |  assign count_offset = {{ADDR_WIDTH + 1 - COUNT_WIDTH{1'b0}}, count};
       |  assign deq_fire_offset = deq_fire_count[ADDR_WIDTH:0];
       |  assign next_count = next_count_ext[COUNT_WIDTH - 1:0];
       |  assign tail = add_mod_depth(head, count_offset);
       |  assign next_head = add_mod_depth(head, deq_fire_offset);
       |  assign next_count_ext = {1'b0, count} + enq_fire_count - deq_fire_count;
       |
       |  logic [ADDR_WIDTH:0] enq_valid_prefix_term [ENQ_NUM];
       |  logic [ADDR_WIDTH:0] enq_valid_prefix_red [ENQ_NUM + 1];
       |  logic [ADDR_WIDTH:0] enq_rank [ENQ_NUM];
       |  logic [ADDR_WIDTH - 1:0] enq_addr [ENQ_NUM];
       |  logic [ENQ_NUM - 1:0] enq_fire_mask;
       |
       |  // Chisel Equivalent:
       |  // enq_valid_prefix_red(k) := PopCount(enq_valid.take(k)) // 0 <= k <= enqNum; k == 0 is 0
       |  // val enq_rank = if (enqIsCompact) VecInit((0 until enqNum).map(_.U))
       |  //                else VecInit((0 until enqNum).map(k => enq_valid_prefix_red(k)))
       |  // enq_ready := VecInit(enq_rank.map(_ < free_count))
       |  // Share this scan across all sparse-lane ranks instead of counting each prefix separately.
       |  always_comb begin
       |    enq_valid_prefix_red[0] = '0;
       |    for (integer e = 0; e < ENQ_NUM; e = e + 1) begin : enq_lane
       |      enq_valid[e] = io_enq_valid[e];
       |      enq_bits[e] = io_enq_bits[e * DATA_WIDTH +: DATA_WIDTH];
       |      enq_mux_data[e * DATA_WIDTH +: DATA_WIDTH] = enq_bits[e];
       |
       |      enq_valid_prefix_term[e] = {{ADDR_WIDTH{1'b0}}, enq_valid[e]};
       |      enq_valid_prefix_red[e + 1] =
       |        enq_valid_prefix_red[e] + enq_valid_prefix_term[e];
       |
       |      if (ENQ_COMPACT != 0) begin : compact_rank
       |        enq_rank[e] = e[ADDR_WIDTH:0];
       |      end else begin : sparse_rank
       |        enq_rank[e] = enq_valid_prefix_red[e];
       |      end
       |
       |      enq_ready[e] = enq_rank[e] < free_count;
       |      io_enq_ready[e] = enq_ready[e];
       |      enq_fire[e] = enq_valid[e] && enq_ready[e];
       |      enq_addr[e] = add_mod_depth(tail, enq_rank[e]);
       |
       |      enq_fire_mask[e] = enq_fire[e];
       |    end
       |  end
       |
       |  // Chisel Equivalent: enq_fire_count := PopCount(enq_fire)
       |  wire [ENQ_COUNT_WIDTH - 1:0] enq_fire_popcount;
       |  ${ctx.ref(SvPopcountImpl)} #(.INPUT_SIZE(ENQ_NUM)) u_enq_fire_count (
       |    .io_in(enq_fire_mask), .io_out(enq_fire_popcount)
       |  );
       |  assign enq_fire_count = {{(COUNT_WIDTH + 1 - ENQ_COUNT_WIDTH){1'b0}}, enq_fire_popcount};
       |
       |  logic [DEQ_NUM - 1:0] deq_fire_mask;
       |  always_comb begin
       |    for (integer i = 0; i < DEQ_NUM; i = i + 1) begin : deq_lane
       |      deq_ready[i] = io_deq_ready[i];
       |      io_deq_valid[i] = deq_valid[i];
       |      io_deq_bits[i * DATA_WIDTH +: DATA_WIDTH] = deq_bits[i];
       |      deq_fire[i] = deq_valid[i] && deq_ready[i];
       |
       |      deq_fire_mask[i] = deq_fire[i];
       |    end
       |  end
       |  // Chisel Equivalent: deq_fire_count := PopCount(deq_fire)
       |  wire [DEQ_COUNT_WIDTH - 1:0] deq_fire_popcount;
       |  ${ctx.ref(SvPopcountImpl)} #(.INPUT_SIZE(DEQ_NUM)) u_deq_fire_count (
       |    .io_in(deq_fire_mask), .io_out(deq_fire_popcount)
       |  );
       |  assign deq_fire_count = {{(COUNT_WIDTH + 1 - DEQ_COUNT_WIDTH){1'b0}}, deq_fire_popcount};
       |
       |  logic [ADDR_WIDTH - 1:0] next_addr [DEQ_NUM];
       |  always_comb begin
       |    for (integer i = 0; i < DEQ_NUM; i = i + 1) begin : deq_addr
       |      next_addr[i] = add_mod_depth(next_head, i[ADDR_WIDTH:0]);
       |    end
       |  end
       |
       |  logic mem_write [DEPTH];
       |  wire [DATA_WIDTH - 1:0] mem_write_data [DEPTH];
       |  logic [ENQ_NUM - 1:0] write_match [DEPTH];
       |  // Chisel Equivalent:
       |  // write_match(i) := VecInit(enq_fire.indices.map(j => enq_fire(j) && enq_addr(j) === i.U)).asUInt
       |  always_comb begin
       |    for (integer i = 0; i < DEPTH; i = i + 1) begin : write_lane
       |      for (integer j = 0; j < ENQ_NUM; j = j + 1) begin : write_match_lane
       |        write_match[i][j] = enq_fire[j] && (enq_addr[j] == i[ADDR_WIDTH - 1:0]);
       |      end
       |      // Chisel Equivalent: mem_write(i) := write_match(i).orR
       |      mem_write[i] = |write_match[i];
       |    end
       |  end
       |  generate
       |    for (genvar i = 0; i < DEPTH; i = i + 1) begin : gen_write_mux
       |      // Chisel Equivalent: mem_write_data(i) := Mux1H(write_match(i), enq_bits)
       |      ${ctx.ref(SvMux1HImpl)} #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(ENQ_NUM)) u_data (
       |        .io_sel(write_match[i]), .io_in(enq_mux_data), .io_out(mem_write_data[i])
       |      );
       |    end
       |  endgenerate
       |
       |  logic [ENQ_NUM - 1:0] deq_forward_match [DEQ_NUM];
       |  wire [DATA_WIDTH - 1:0] deq_forward_data [DEQ_NUM];
       |  logic [DATA_WIDTH - 1:0] deq_next_bits [DEQ_NUM];
       |  // Chisel Equivalent:
       |  // deq_forward_match(i) := VecInit(enq_fire.indices.map(j => enq_fire(j) && (enq_addr(j) === next_addr(i)))).asUInt
       |  // val hit = deq_forward_match(i).orR
       |  // deq_forward_data(i) := Mux1H(deq_forward_match(i), enq_bits)
       |  // hit implies the packed match mask is exactly one-hot; zero-hit uses mem.
       |  always_comb begin
       |    for (integer i = 0; i < DEQ_NUM; i = i + 1) begin : forward_lane
       |      for (integer j = 0; j < ENQ_NUM; j = j + 1) begin : forward_match_lane
       |        deq_forward_match[i][j] = enq_fire[j] && (enq_addr[j] == next_addr[i]);
       |      end
       |    end
       |  end
       |  // Chisel Equivalent:
       |  // deq_next_bits(i) := Mux(hit, deq_forward_data(i), mem(next_addr(i)))
       |  always_comb begin
       |    for (integer i = 0; i < DEQ_NUM; i = i + 1) begin : deq_forward_output
       |      deq_next_bits[i] = (|deq_forward_match[i]) ? deq_forward_data[i] : mem[next_addr[i]];
       |    end
       |  end
       |  generate
       |    for (genvar i = 0; i < DEQ_NUM; i = i + 1) begin : gen_forward_mux
       |      ${ctx.ref(SvMux1HImpl)} #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(ENQ_NUM)) u_data (
       |        .io_sel(deq_forward_match[i]), .io_in(enq_mux_data), .io_out(deq_forward_data[i])
       |      );
       |    end
       |  endgenerate
       |
       |  // Payload memory is intentionally unreset.
       |  always_ff @(posedge clock) begin
       |    for (integer i = 0; i < DEPTH; i = i + 1) begin
       |      if (mem_write[i]) mem[i] <= mem_write_data[i];
       |    end
       |  end
       |
       |  always_ff @(posedge clock or posedge reset) begin
       |    if (reset) begin
       |      head <= '0;
       |      count <= '0;
       |      for (integer i = 0; i < DEQ_NUM; i = i + 1) deq_valid[i] <= 1'b0;
       |    end else begin
       |      head <= next_head;
       |      count <= next_count;
       |      for (integer i = 0; i < DEQ_NUM; i = i + 1) begin
       |        deq_valid[i] <= i < next_count_ext;
       |        if (i < next_count_ext) begin
       |          deq_bits[i] <= deq_next_bits[i];
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
  q.reset := reset

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
