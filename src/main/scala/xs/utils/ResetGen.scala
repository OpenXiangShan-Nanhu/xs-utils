/***************************************************************************************
* Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2022 Peng Cheng Laboratory
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

package xs.utils

import chisel3._
import chisel3.util._
class DFTResetSignals extends Bundle{
  val lgc_rst_n = AsyncReset()
  val mode = Bool()
  val scan_mode = Bool()
}

class ResetGenIO extends Bundle {
  val i_clock = Input(Clock())
  val i_reset = Input(AsyncReset())
  val i_dft_lgc_rst_n = Input(AsyncReset())
  val i_dft_mode = Input(Bool())
  val i_dft_scan_mode = Input(Bool())
  val o_reset = Output(AsyncReset())
  val o_raw_reset = Output(AsyncReset())
}

class ResetGenInner(SYNC_NUM: Int = 2) extends BlackBox with HasBlackBoxInline {
  require(SYNC_NUM > 1)
  val io = IO(new ResetGenIO)
  private val modName = s"${GlobalData.prefix}ResetGenInnerS${SYNC_NUM}"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""// VCS coverage exclude_file
       |module $modName (
       |  input  i_clock,
       |  input  i_reset,
       |  input  i_dft_lgc_rst_n,
       |  input  i_dft_mode,
       |  input  i_dft_scan_mode,
       |  output o_reset,
       |  output o_raw_reset
       |);
       |  wire reset = i_dft_mode ? ~i_dft_lgc_rst_n : i_reset;
       |  reg [${SYNC_NUM - 1}:0] shifter;
       |
       |`ifndef SYNTHESIS
       |  initial shifter = ${SYNC_NUM}'d${(1 << SYNC_NUM) - 1};
       |`endif
       |
       |  always @(posedge i_clock or posedge reset) begin
       |    if (reset) begin
       |      shifter <= ${SYNC_NUM}'d${(1 << SYNC_NUM) - 1};
       |    end else begin
       |      shifter <= {1'b0, shifter[${SYNC_NUM - 1}:1]};
       |    end
       |  end
       |  assign o_raw_reset = shifter[0];
       |  assign o_reset = i_dft_scan_mode ? ~i_dft_lgc_rst_n : shifter[0];
       |endmodule""".stripMargin)
}

class ResetGen(SYNC_NUM: Int = 2) extends Module {
  override val desiredName = s"ResetGenS${SYNC_NUM}"
  val o_reset = IO(Output(AsyncReset()))
  val dft = IO(Input(new DFTResetSignals()))
  val raw_reset = IO(Output(AsyncReset()))

  private val inner = Module(new ResetGenInner(SYNC_NUM))
  inner.io.i_reset := reset
  inner.io.i_clock := clock
  inner.io.i_dft_lgc_rst_n := dft.lgc_rst_n
  inner.io.i_dft_scan_mode := dft.scan_mode
  inner.io.i_dft_mode := dft.mode
  raw_reset := inner.io.o_raw_reset
  o_reset := inner.io.o_reset
}

trait ResetNode

case class ModuleNode(mod: Module) extends ResetNode
case class CellNode(reset: Reset) extends ResetNode
case class ResetGenNode(children: Seq[ResetNode]) extends ResetNode

object ResetGen {
  def apply(SYNC_NUM: Int = 2, dft:Option[DFTResetSignals]): AsyncReset = {
    val resetSync = Module(new ResetGen(SYNC_NUM))
    if(dft.isDefined) {
      resetSync.dft := dft.get
    } else {
      resetSync.dft := 0.U.asTypeOf(new DFTResetSignals)
    }
    resetSync.o_reset
  }

  def apply(resetTree: ResetNode, reset: Reset, dft:Option[DFTResetSignals], sim: Boolean): Unit = {
    if(!sim) {
      resetTree match {
        case ModuleNode(mod) =>
          mod.reset := reset
        case CellNode(r) =>
          r := reset
        case ResetGenNode(children) =>
          val next_rst = Wire(Reset())
          withReset(reset){
            val resetGen = Module(new ResetGen)
            next_rst := resetGen.o_reset
            if(dft.isDefined) {
              resetGen.dft := dft.get
            } else {
              resetGen.dft := 0.U.asTypeOf(new DFTResetSignals)
            }
          }
          children.foreach(child => apply(child, next_rst, dft, sim))
      }
    }
  }

  def apply(resetChain: Seq[Seq[Module]], reset: Reset,  dft:Option[DFTResetSignals], sim: Boolean): Seq[Reset] = {
    val resetReg = Wire(Vec(resetChain.length + 1, Reset()))
    resetReg.foreach(_ := reset)
    for ((resetLevel, i) <- resetChain.zipWithIndex) {
      if (!sim) {
        withReset(resetReg(i)) {
          val resetGen = Module(new ResetGen)
          resetReg(i + 1) := resetGen.o_reset
          if(dft.isDefined) {
            resetGen.dft := dft.get
          } else {
            resetGen.dft := 0.U.asTypeOf(new DFTResetSignals)
          }
        }
      }
      resetLevel.foreach(_.reset := resetReg(i + 1))
    }
    resetReg.tail
  }

  def applyOneLevel(resetSigs: Seq[Reset], reset: Reset, sim: Boolean): DFTResetSignals = {
    val resetReg = Wire(Reset())
    val dft = Wire(new DFTResetSignals())
    resetReg := reset
    if (!sim) {
      withReset(reset) {
        val resetGen = Module(new ResetGen)
        resetReg := resetGen.o_reset
        resetGen.dft := dft
      }
    }
    resetSigs.foreach(_ := resetReg)
    dft
  }
}
