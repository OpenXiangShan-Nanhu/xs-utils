package xs.utils.arb

import chisel3._
import chisel3.util._

class SelNto1[T <: Data](gen: T, size:Int, selfCmpOtherFunc: (T, T) => Bool) extends Module {
  val io = IO(new Bundle {
    val in = Vec(size, Flipped(Valid(gen)))
    val out = Output(UInt(size.W))
  })
  private val oldestOhSeq = io.in.zipWithIndex.map({ case (self, idx) =>
    val cmpVec = io.in.zipWithIndex.filterNot(_._2 == idx).map(i => self.valid && Mux(i._1.valid, selfCmpOtherFunc(self.bits, i._1.bits), true.B))
    Cat(cmpVec).andR
  })
  private val valids = Cat(io.in.map(_.valid).reverse)
  io.out := Cat(oldestOhSeq.reverse)
  assert((valids & io.out) === io.out, "selected entries should all be valid!")
}
