package xs.utils.arb

import chisel3._
import chisel3.util._

class ConditionVipArbiter[T <: Data](gen: T, size:Int, selfCmpOtherFunc: (T, T) => Bool) extends Module {
  val io = IO(new Bundle {
    val in = Vec(size, Flipped(Decoupled(gen)))
    val out = Decoupled(gen)
    val chosen = Output(UInt(log2Ceil(size).W))
  })
  private val selector = Module(new SelNto1(gen, size, selfCmpOtherFunc))
  private val selReg = RegNext(selector.io.out) // Do not gate this reg
  private val selArb = Module(new VipArbiter(gen, size))

  for(i <- io.in.indices) {
    selector.io.in(i).valid := io.in(i).valid && !selArb.io.in(i).fire
    selector.io.in(i).bits := io.in(i).bits
    io.in(i).ready := selArb.io.in(i).fire

    selArb.io.in(i).valid := selReg(i)
    selArb.io.in(i).bits := io.in(i).bits
  }
  io.chosen := selArb.io.chosen
  io.out <> selArb.io.out
}