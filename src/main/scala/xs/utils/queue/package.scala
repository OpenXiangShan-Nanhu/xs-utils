package xs.utils

import chisel3.Data

/** Source-compatible aliases; implementations and inline wrappers live in verilog.
  * These aliases do not introduce subclasses, modules, or extra RTL hierarchy.
  */
package object queue {
  type FastQueue[T <: Data] = _root_.xs.utils.verilog.SvFastQueue[T]
  val FastQueue: _root_.xs.utils.verilog.SvFastQueue.type = _root_.xs.utils.verilog.SvFastQueue

  type FastQueueFactory = _root_.xs.utils.verilog.SvFastQueueFactory
  val FastQueueRaw: _root_.xs.utils.verilog.SvFastQueueRaw.type = _root_.xs.utils.verilog.SvFastQueueRaw
  val FastQueueNoX: _root_.xs.utils.verilog.SvFastQueueNoX.type = _root_.xs.utils.verilog.SvFastQueueNoX

  type MimoQueue[T <: Data] = _root_.xs.utils.verilog.SvMimoQueue[T]
}
