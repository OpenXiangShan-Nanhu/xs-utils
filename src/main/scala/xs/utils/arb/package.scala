package xs.utils

import chisel3.Data

/** Compatibility aliases introduce no extra wrapper or RTL hierarchy. */
package object arb {
  type VipArbiter[T <: Data] = _root_.xs.utils.verilog.SvVipArbiter[T]
  val VipArbiter: _root_.xs.utils.verilog.SvVipArbiter.type = _root_.xs.utils.verilog.SvVipArbiter

  type StepVipArbiter[T <: Data] = VipArbiter[T]
  val StepVipArbiter: _root_.xs.utils.verilog.SvVipArbiter.type = VipArbiter

  type RRArbiter[T <: Data] = _root_.xs.utils.verilog.SvRRArbiter[T]
  val RRArbiter: _root_.xs.utils.verilog.SvRRArbiter.type = _root_.xs.utils.verilog.SvRRArbiter

  type BaseVipArbiter[T <: Data] = VipArbiter[T]
}
