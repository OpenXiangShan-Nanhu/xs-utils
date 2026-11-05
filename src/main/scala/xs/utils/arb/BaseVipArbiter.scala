package xs.utils.arb

import chisel3.Data
import chisel3.experimental.SourceInfo
import chisel3.util.DecoupledIO

/** Retains the legacy factory; normal and step now use the same VIP policy. */
object BaseVipArbiter {
  def apply[T <: Data](in: Seq[DecoupledIO[T]], name: String = "normal", selLatch: Boolean = false)(
    implicit sourceInfo: SourceInfo
  ): DecoupledIO[T] = VipArbiter(in, selLatch)
}
