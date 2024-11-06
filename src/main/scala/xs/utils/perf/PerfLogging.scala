/** *************************************************************************************
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
  * *************************************************************************************
  */

package xs.utils.perf

import chisel3._
import org.chipsalliance.cde.config.Parameters

trait HasPerfLogging {
  this: RawModule with ImplicitClock with ImplicitReset =>
  val p: Parameters
  private val perfDump = WireInit(false.B)
  private val perfClean = WireInit(false.B)
  private val logEnable = WireInit(false.B)
  private val logTimestamp = WireInit(0.U(64.W))
  private val env = p(DebugOptionsKey)
  private val perfEnable = env.EnablePerfDebug && !env.FPGAPlatform
  private val logHelper = if (perfEnable) Some(Module(new LogHelper)) else None
  logHelper.foreach(m => {
    perfDump := m.io.dump
    perfClean := m.io.clean
    logEnable := m.io.logEnable
    logTimestamp := m.io.timer
  })
  val XSDebug = new XSLogger(logEnable, logTimestamp, XSLogLevel.DEBUG)(p)
  val XSInfo = new XSLogger(logEnable, logTimestamp, XSLogLevel.INFO)(p)
  val XSWarn = new XSLogger(logEnable, logTimestamp, XSLogLevel.WARN)(p)
  val XSError = new XSLogger(logEnable, logTimestamp, XSLogLevel.ERROR)(p)
  val XSPerf = new XSLogger(logEnable, logTimestamp, XSLogLevel.PERF)(p)
  def XSPerfPrint(pable: Printable): Any = XSPerf(true, true.B, pable)

  def XSPerfAccumulate(perfName: String, perfCnt: UInt): Unit = {
    if (perfEnable) {
      val counter = RegInit(0.U(64.W))
      val next_counter = counter + perfCnt
      counter := Mux(perfClean, 0.U, next_counter)
      when(perfDump) {
        XSPerfPrint(p"$perfName, $next_counter\n")
      }
    }
  }

  def XSPerfHistogram(
    perfName:     String,
    perfCnt:      UInt,
    enable:       Bool,
    start:        Int,
    stop:         Int,
    step:         Int,
    left_strict:  Boolean = false,
    right_strict: Boolean = false
  ): Unit = {
    if (perfEnable) {
      // drop each perfCnt value into a bin
      val nBins = (stop - start) / step
      require(start >= 0)
      require(stop > start)
      require(nBins > 0)

      (0 until nBins).map { i =>
        val binRangeStart = start + i * step
        val binRangeStop = start + (i + 1) * step
        val inRange = perfCnt >= binRangeStart.U && perfCnt < binRangeStop.U

        // if perfCnt < start, it will go to the first bin
        val leftOutOfRange = if (left_strict) {
          false.B
        } else {
          perfCnt < start.U && i.U === 0.U
        }
        // if perfCnt >= stop, it will go to the last bin
        val rightOutOfRange = if (right_strict) {
          false.B
        } else {
          perfCnt >= stop.U && i.U === (nBins - 1).U
        }
        val inc = inRange || leftOutOfRange || rightOutOfRange

        val counter = RegInit(0.U(64.W))
        when(perfClean) {
          counter := 0.U
        }.elsewhen(enable && inc) {
          counter := counter + 1.U
        }

        when(perfDump) {
          XSPerfPrint(p"${perfName}_${binRangeStart}_${binRangeStop}, $counter\n")
        }
      }
    }
  }

  def XSPerfMax(perfName: String, perfCnt: UInt, enable: Bool): Unit = {
    if (perfEnable) {
      val max = RegInit(0.U(64.W))
      val next_max = Mux(enable && (perfCnt > max), perfCnt, max)
      max := Mux(perfClean, 0.U, next_max)
      when(perfDump) {
        XSPerfPrint(p"${perfName}_max, $next_max\n")
      }
    }
  }

  def QueuePerf(size: Int, utilization: UInt, full: UInt): Unit = {
    XSPerfAccumulate("utilization", utilization)
    XSPerfHistogram("util", utilization, true.B, 0, size, 1)
    XSPerfAccumulate("full", full)
    val exHalf = utilization > (size / 2).U
    val empty = utilization === 0.U
    XSPerfAccumulate("exHalf", exHalf)
    XSPerfAccumulate("empty", empty)
  }

  def TransactionLatencyCounter(start: Bool, stop: Bool): (Bool, UInt) = {
    assert(!(start && stop))
    val counter = RegInit(0.U(64.W))
    val next_counter = counter + 1.U
    counter := Mux(start || stop, 0.U, next_counter)
    (stop, next_counter)
  }
}
