package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.mbist.{Mbist, Ram2Mbist, Ram2MbistParams}

class SpSramReq[T <: Data](gen: T, set:Int, way:Int) extends Bundle {
  val addr = UInt(log2Ceil(set).W)
  val mask = if(way == 1) None else Some(UInt(way.W))
  val write = Bool()
  val data = Vec(way, gen)
}

class SpSramResp[T <: Data](gen: T, way:Int) extends Bundle {
  val data = Vec(way, gen)
}

class SinglePortSramTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  shouldReset: Boolean = false,
  holdRead: Boolean = false,
  setup:Int = 1, // ask your leader to changed this
  latency: Int = 1, // ask your leader to changed this
  extraHold: Boolean = false,  //ask your leader to changed this
  hasMbist: Boolean = false,
  suffix: String = "",
  powerCtl: Boolean = false,
  foundry: String = "Unknown",
  sramInst: String = "STANDARD"
) extends Module {
  private val hold = if(extraHold) setup + 1 else setup
  private val sp = SramInfo(gen.getWidth, way, hasMbist)
  private val ram = Module(new SRAMTemplate(
    gen = UInt(sp.sramSegBits.W),
    set = set,
    way = sp.sramMaskBits,
    singlePort = true,
    shouldReset = shouldReset,
    extraReset = false,
    holdRead = holdRead,
    bypassWrite = false,
    setup = setup,
    latency = latency,
    extraHold = extraHold,
    hasMbist = false,
    explictBist = hasMbist,
    suffix = suffix,
    powerCtl = powerCtl,
    foundry = foundry,
    sramInst = sramInst
  ))

  private val mbp = Ram2MbistParams(sp, set, singlePort = true, ram.sramName, "", foundry, sramInst, if(hold > 1) 1 else 0, "None", this)
  val io = IO(new Bundle{
    val req = Flipped(Decoupled(new SpSramReq(gen, set, way)))
    val resp = Valid(new SpSramResp(gen, way))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
    val broadcast = if(hasMbist) Some(new SramBroadcastBundle) else None
    val mbist = if(hasMbist) Some(new Ram2Mbist(mbp)) else None
  })
  private val finalReq = Wire(Decoupled(new SpSramReq(UInt(sp.sramSegBits.W), set, sp.sramMaskBits)))
  finalReq.valid := io.req.valid
  finalReq.bits.addr := io.req.bits.addr
  finalReq.bits.write := io.req.bits.write
  finalReq.bits.mask.foreach(_ := sp.funcMaskConverse(io.req.bits.mask.getOrElse(Fill(way, true.B))))
  finalReq.bits.data := io.req.bits.data.asTypeOf(finalReq.bits.data)
  io.req.ready := finalReq.ready

  if(hasMbist) {
    ram.io.broadcast.get := io.broadcast.get
    io.broadcast.get := DontCare
    dontTouch(io.broadcast.get)
    SramHelper.broadCastBdQueue.enqueue(io.broadcast.get)
    io.mbist.get := DontCare
    dontTouch(io.mbist.get)
    Mbist.addRamNode(io.mbist.get, sp.mbistArrayIds)
    when(io.mbist.get.ack) {
      finalReq.valid := io.mbist.get.we || io.mbist.get.re
      finalReq.bits.addr := io.mbist.get.addr_rd
      finalReq.bits.write := io.mbist.get.we
      finalReq.bits.mask.foreach(_ := sp.mbistMaskConverse(io.mbist.get.wmask, io.mbist.get.selectedOH))
      finalReq.bits.data := io.mbist.get.wdata.asTypeOf(finalReq.bits.data)
    }
    io.mbist.get.rdata := ram.io.r.resp.data.asUInt
  }

  ram.io.pwctl.foreach(_ := io.pwctl.get)
  ram.io.broadcast.foreach(_ := io.broadcast.get)
  io.resp.valid := ram.io.r.resp.valid
  io.resp.bits.data := ram.io.r.resp.data.asTypeOf(io.resp.bits.data)

  ram.io.r.req.valid := finalReq.valid && !finalReq.bits.write
  ram.io.w.req.valid := finalReq.valid && finalReq.bits.write
  finalReq.ready := ram.io.w.req.ready

  private val addr = if(hold > 1) RegEnable(finalReq.bits.addr, finalReq.fire) else finalReq.bits.addr
  private val data = if(hold > 1) RegEnable(finalReq.bits.data, finalReq.fire && finalReq.bits.write) else finalReq.bits.data
  private val mask = finalReq.bits.mask.map(m => {
    if(hold > 1) {
      RegEnable(m, finalReq.fire && finalReq.bits.write)
    } else {
      m
    }
  })
  ram.io.r.req.bits.setIdx := addr
  ram.io.w.req.bits.setIdx := addr
  ram.io.w.req.bits.data := data
  ram.io.w.req.bits.waymask.foreach(_ := mask.get)
}
