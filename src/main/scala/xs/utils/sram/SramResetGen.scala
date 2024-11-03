package xs.utils.sram

import chisel3._
import chisel3.util._

class SramResetGen(
  set: Int,
  interval: Int = 1,
  resetDelay: Int = 4
) extends Module {
  private val setBits = log2Ceil(set)
  val io = IO(new Bundle {
    val resetState = Output(Bool())
    val waddr = Output(UInt(setBits.W))
    val wen = Output(Bool())
  })
  private val resetCounter = RegInit((set - 1).U(setBits.W))
  private val resetState = RegInit(true.B)
  private val resetVal = ((0x1L << resetDelay) - 1).U(resetDelay.W)
  private val resetHold = RegInit(resetVal)
  private val intervalCounter = RegInit(0.U(log2Ceil(interval + 1).W))
  resetHold := Cat(0.U(1.W), resetHold(resetDelay - 1, 1))

  io.wen := resetState && !resetHold(0) && intervalCounter === 0.U

  when(io.wen) {
    intervalCounter := (interval - 1).U
  }.elsewhen(intervalCounter.orR) {
    intervalCounter := intervalCounter - 1.U
  }

  when(io.wen) {
    resetCounter := Mux(resetCounter === 0.U, 0.U, resetCounter - 1.U)
    resetState := resetCounter =/= 0.U
  }

  if(interval > 1) {
    io.waddr := RegEnable(resetCounter, io.wen)
    io.resetState := RegNext(resetState || intervalCounter.orR)
  } else {
    io.waddr := resetCounter
    io.resetState := resetState
  }
}
