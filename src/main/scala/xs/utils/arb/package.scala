package xs.utils

import chisel3.Data

/** Compatibility aliases introduce no extra wrapper or RTL hierarchy. */
package object arb {
  type VipArbiter[T <: Data] = _root_.xs.utils.verilog.SvVipArbiter[T]
  val VipArbiter: _root_.xs.utils.verilog.SvVipArbiter.type = _root_.xs.utils.verilog.SvVipArbiter

  type RRArbiter[T <: Data] = _root_.xs.utils.verilog.SvRRArbiter[T]
  val RRArbiter: _root_.xs.utils.verilog.SvRRArbiter.type = _root_.xs.utils.verilog.SvRRArbiter
}
