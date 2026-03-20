package xs.utils.queue

import chisel3._
import chisel3.util._


class FastQueue[T <: Data](gen:T, size:Int, deqDataNoX:Boolean = false) extends Module with HasCircularQueuePtrHelper {
  require(size >= 2)
  val io = IO(new Bundle {
    val enq     = Flipped(Decoupled(gen))
    val deq     = Decoupled(gen)
    val count   = Output(UInt(log2Ceil(size + 1).W))
    val freeNum = Output(UInt(log2Ceil(size + 1).W))
  })

  private val driver    = Module(new Queue(gen = gen, entries = 1, pipe = true, flow = false))
  private val waterline = RegInit(1.U((size + 1).W))
  private val full      = waterline(size)

  io.deq       <> driver.io.deq
  io.enq.ready := !full

  if(size > 2) {
    val holder = Module(new Queue(gen = gen, entries = 1, pipe = true, flow = false))
    val squeue = Module(new Queue(gen = gen, entries = size - 2, pipe = size == 3, flow = true))

    squeue.io.deq.ready := driver.io.enq.ready
    driver.io.enq.valid := io.enq.valid || squeue.io.deq.valid
    when(squeue.io.deq.valid) {
      driver.io.enq.bits := squeue.io.deq.bits
    }.otherwise {
      driver.io.enq.bits := io.enq.bits
    }

    // When count==1 and driver can take io.enq directly during a simultaneous
    // deq/enq cycle, do not duplicate the same flit into holder.
    holder.io.enq.valid := io.enq.valid && !waterline(0) && !(waterline(1) && driver.io.enq.ready)
    holder.io.enq.bits  := io.enq.bits
    squeue.io.enq       <> holder.io.deq
  } else {
    val holder = Module(new Queue(gen = gen, entries = 1, pipe = false, flow = true))
    driver.io.enq <> holder.io.deq
    holder.io.enq <> io.enq
    io.enq.ready  := !full
  }

  if(deqDataNoX) {
    io.deq.bits := Mux(driver.io.deq.valid, driver.io.deq.bits, 0.U.asTypeOf(gen))
  }

  private val ptrMoveVec = Cat(io.enq.fire, io.deq.fire)
  when(ptrMoveVec === "b01".U) {
    waterline := Cat(false.B, waterline(size, 1))
  }.elsewhen(ptrMoveVec === "b10".U) {
    waterline := Cat(waterline(size - 1, 0), false.B)
  }
  assert(PopCount(waterline) === 1.U)

  io.count   := Mux1H(Seq.tabulate(size + 1)(i => (waterline(i), i.U)))
  io.freeNum := Mux1H(Seq.tabulate(size + 1)(i => (waterline(i), (size - i).U)))
}

object FastQueueRaw {
  def apply[T <: Data](enq: DecoupledIO[T], size: Int, noX:Boolean, name:Option[String]): DecoupledIO[T] = {
    val q = Module(new FastQueue(chiselTypeOf(enq.bits), size, noX))
    name.foreach(n => q.suggestName(n))
    q.io.enq <> enq
    q.io.deq
  }
}

class FastQueueFactory(noX:Boolean) {
  def apply[T <: Data](enq: DecoupledIO[T], size: Int, name:Option[String]): DecoupledIO[T] = FastQueueRaw(enq, size, noX, name)

  def apply[T <: Data](enq: DecoupledIO[T], name:Option[String]): DecoupledIO[T] = apply(enq, 2, name)

  def apply[T <: Data](enq: DecoupledIO[T], size: Int): DecoupledIO[T] = apply(enq, size, None)

  def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T] = apply(enq, 2, None)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], size: Int, name:Option[String]):Unit = deq <> apply(enq, size, name)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], size: Int):Unit = deq <> apply(enq, size, None)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], name:Option[String]):Unit = deq <> apply(enq, 2, name)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T]):Unit = deq <> apply(enq, 2, None)

  def apply[T <: Data](enq: DecoupledIO[T], enable: Boolean, size: Int): DecoupledIO[T] = if(enable) apply(enq, size, None) else enq

  def apply[T <: Data](enq: DecoupledIO[T], enable: Boolean): DecoupledIO[T] = apply(enq, enable, 2)
}

object FastQueueNoX extends FastQueueFactory(true)

object FastQueue extends FastQueueFactory(false)
