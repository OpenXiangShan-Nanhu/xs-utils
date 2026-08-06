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

package xs.utils.perf

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._

class PerfEvent() extends Bundle {
  val value = UInt(6.W)
  val id    = UInt(10.W)
}
object PerfEvent {
  def apply(value: UInt, id: UInt): PerfEvent = {
    val pe = Wire(new PerfEvent)
    pe.value := value
    pe.id    := id
    pe
  }
}

trait HasPerfEvents { this: RawModule =>
  val perfEvents: Seq[(String, UInt)]

  lazy val io_perf: Vec[PerfEvent] = IO(Output(Vec(perfEvents.length, new PerfEvent)))
  def generatePerfEvent(noRegNext: Option[Seq[Int]] = None): Unit = {
    io_perf := DontCare
    for (((out, (name, counter)), i) <- io_perf.zip(perfEvents).zipWithIndex) {
      require(!name.contains("/"))
      out.value := RegNext(RegNext(counter))
      if (noRegNext.isDefined && noRegNext.get.contains(i)) {
        out.value := counter
      }
    }
  }
  def getPerfEvents: Seq[(String, UInt)] = {
    perfEvents.map(_._1).zip(io_perf).map(x => (x._1, x._2.value))
  }
  def getPerf: Vec[PerfEvent] = io_perf
}

class HPerfCounter(val numPCnt: Int)(implicit p: Parameters) extends Module with HasPerfEvents {
  val io = IO(new Bundle {
    val hpm_event   = Input(UInt(64.W))
    val events_sets = Input(Vec(numPCnt, new PerfEvent))
  })

  // Only use the lower 10 bits of hpm_event as the event selector.
  // Higher bits (multi-event select / combine ops) are ignored.
  val eventsMap = io.events_sets.map(event => (event.id, event.value))
  val selected = RegNext(MuxLookup(io.hpm_event(9, 0), 0.U)(eventsMap))

  val perfEvents = Seq(("selected", selected))
  generatePerfEvent()
}

class HPerfMonitor(numCSRPCnt: Int, numPCnt: Int)(implicit p: Parameters) extends Module with HasPerfEvents {
  val io = IO(new Bundle {
    val hpm_event   = Input(Vec(numCSRPCnt, UInt(64.W)))
    val events_sets = Input(Vec(numPCnt, new PerfEvent))
  })

  val perfEvents = io.hpm_event.zipWithIndex.map{ case (hpm, i) =>
    val hpc = Module(new HPerfCounter(numPCnt))
    hpc.io.events_sets <> io.events_sets
    hpc.io.hpm_event   := hpm
    val selected = hpc.getPerfEvents.head
    (s"${selected._1}_$i", selected._2)
  }
  generatePerfEvent()
}

object HPerfMonitor {
  def apply(hpm_event: Seq[UInt], events_sets: Seq[PerfEvent])(implicit p: Parameters): HPerfMonitor = {
    val hpm = Module(new HPerfMonitor(hpm_event.length, events_sets.length))
    hpm.io.hpm_event := hpm_event
    hpm.io.events_sets := events_sets
    hpm
  }
}