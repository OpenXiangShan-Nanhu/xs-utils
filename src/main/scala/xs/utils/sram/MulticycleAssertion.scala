package xs.utils.sram

import chisel3._
import xs.utils.GlobalData

class SetupMulticycleAssert(D:Int, S:Int) extends ExtModule(Map(
  "D" -> D
)) {
  val io = FlatIO(new Bundle {
    val i_clk = Input(Clock())
    val i_rst = Input(Reset())
    val i_dat = Input(UInt(D.W))
    val i_con = Input(Bool())
  })
  require(S > 1)
  private val modName = s"${GlobalData.prefix}_SVA_SetupMulticycle$S"
  override val desiredName = modName

  private val mcpChkStr = Seq.tabulate(S - 1)(i => s"i_dat === $$past(i_dat, ${i + 1})").mkString("(", " && ", ");")
  setInline(s"$modName.sv",
    s"""
       |// VCS coverage exclude_file
       |module $modName #(
       |  parameter int D = $D
       |)(
       |  input wire         i_clk,
       |  input wire         i_rst,
       |  input wire         i_con,
       |  input wire [D-1:0] i_dat
       |);
       |`ifndef VERILATOR
       |  property setup_multicycle_${S};
       |    @(posedge i_clk) disable iff(i_rst)
       |    (i_con) |-> $mcpChkStr
       |  endproperty
       |
       |  assert property (setup_multicycle_${S}) else begin
       |    $$error("Assertion Failed in %m @ %t: Setup multicycle checking failed!", $$time);
       |    $$fatal;
       |  end
       |`endif
       |endmodule""".stripMargin)
}

class HoldMulticycleAssert(D:Int) extends ExtModule(Map(
  "D" -> D
)) {
  val io = FlatIO(new Bundle {
    val i_clk = Input(Clock())
    val i_rst = Input(Reset())
    val i_dat = Input(UInt(D.W))
    val i_con = Input(Bool())
  })
  private val modName = s"${GlobalData.prefix}_SVA_HoldMulticycle"
  override val desiredName = modName

  setInline(s"$modName.sv",
    s"""
       |// VCS coverage exclude_file
       |module $modName #(
       |  parameter int D = $D
       |)(
       |  input wire         i_clk,
       |  input wire         i_rst,
       |  input wire         i_con,
       |  input wire [D-1:0] i_dat
       |);
       |`ifndef VERILATOR
       |  property hold_multicycle;
       |    @(posedge i_clk) disable iff(i_rst)
       |    (i_con) |-> ##1 (i_dat === $$past(i_dat, 1));
       |  endproperty
       |
       |  assert property (hold_multicycle) else begin
       |    $$error("Assertion Failed in %m @ %t: Hold multicycle checking failed!", $$time);
       |    $$fatal;
       |  end
       |`endif
       |endmodule""".stripMargin)
}

class LatencyAssert(S:Int) extends ExtModule() {
  val io = FlatIO(new Bundle {
    val i_clk = Input(Clock())
    val i_rst = Input(Reset())
    val i_dat = Input(Bool())
    val i_con = Input(Bool())
  })
  require(S > 1)
  private val modName = s"${GlobalData.prefix}_SVA_Latency$S"
  override val desiredName = modName

  setInline(s"$modName.sv",
    s"""
       |// VCS coverage exclude_file
       |module $modName(
       |  input wire i_clk,
       |  input wire i_rst,
       |  input wire i_con,
       |  input wire i_dat
       |);
       |`ifndef VERILATOR
       |  property latency_multicycle;
       |    @(posedge i_clk) disable iff(i_rst)
       |    (i_con) |-> ##$S i_dat;
       |  endproperty
       |
       |  assert property (latency_multicycle) else begin
       |    $$error("Assertion Failed in %m @ %t: Latency checking failed!", $$time);
       |    $$fatal;
       |  end
       |`endif
       |endmodule""".stripMargin)
}

class EventIntervalAssert(S:Int) extends ExtModule() {
  val io = FlatIO(new Bundle {
    val i_clk = Input(Clock())
    val i_rst = Input(Reset())
    val i_evt = Input(Bool())
  })
  require(S > 1)
  private val modName = s"${GlobalData.prefix}_SVA_EventInterval$S"
  override val desiredName = modName

  private val mcpChkStr = Seq.tabulate(S - 1)(i => s"1'b0 === $$past(i_evt, ${i + 1})").mkString("(", " && ", ");")
  setInline(s"$modName.sv",
    s"""
       |// VCS coverage exclude_file
       |module $modName(
       |  input wire i_clk,
       |  input wire i_rst,
       |  input wire i_evt
       |);
       |`ifndef VERILATOR
       |  property event_interval_${S};
       |    @(posedge i_clk) disable iff(i_rst)
       |    (i_evt) |-> $mcpChkStr
       |  endproperty
       |
       |  assert property (event_interval_${S}) else begin
       |    $$error("Assertion Failed in %m @ %t: Interval checking failed!", $$time);
       |    $$fatal;
       |  end
       |`endif
       |endmodule""".stripMargin)
}