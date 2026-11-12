package xs

import chisel3.Data

package object utils {
  type FastArbiter[T <: Data] = _root_.xs.utils.verilog.SvFastArbiter[T]
  val FastArbiter: _root_.xs.utils.verilog.SvFastArbiter.type = _root_.xs.utils.verilog.SvFastArbiter

  type SRAMQueue[T <: Data] = _root_.xs.utils.sram.SramQueue[T]
  type CircularQueuePtr[T <: CircularQueuePtr[T]] = _root_.xs.utils.queue.CircularQueuePtr[T]
  type MimoQueue[T <: Data] = _root_.xs.utils.queue.MimoQueue[T]
  type OverrideableQueue[T <: Data] = _root_.xs.utils.queue.OverrideableQueue[T]
  type HasCircularQueuePtrHelper = _root_.xs.utils.queue.HasCircularQueuePtrHelper

  class ClockGate extends xs.utils.verilog.ClockGate
  val ClockGate = xs.utils.verilog.ClockGate
  val PerfCCT = xs.utils.perf.TaggedTrace

  class LatchFastArbiter[T <: Data](gen: T, n: Int) extends utils.verilog.SvLatchFastArbiter[T](gen, n)
}
