package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.mbist.{Mbist, Ram2Mbist, Ram2MbistParams}
import xs.utils.sram.SramHelper.genMbistBoreSink

class DpSramWrite[T <: Data](gen: T, set: Int, way: Int) extends Bundle {
  val addr = UInt(log2Ceil(set).W)
  val mask = if(way == 1) None else Some(UInt(way.W))
  val data = Vec(way, gen)
}

class DualPortSramTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  shouldReset: Boolean = false,
  holdRead: Boolean = false,
  bypassWrite: Boolean = false,
  setup: Int = 1,
  latency: Int = 1,
  extraHold: Boolean = false,
  hasMbist: Boolean = false,
  suffix: String = "",
  powerCtl: Boolean = false,
  outputReg: Boolean = false,
  foundry: String = "Unknown",
  sramInst: String = "STANDARD",
  moduleName: Option[String] = None
) extends Module {
  override val desiredName = moduleName.getOrElse("DualPortSramTemplate")

  private val isc = if(extraHold) setup + 1 else setup
  private val sp  = SramInfo(gen.getWidth, way, bist = false)
  private val ram = Module(new SRAMTemplate(
    gen = UInt(sp.sramSegBits.W),
    set = set,
    way = sp.sramMaskBits,
    singlePort = false,
    shouldReset = shouldReset,
    extraReset = false,
    holdRead = holdRead,
    bypassWrite = bypassWrite,
    setup = setup,
    latency = latency,
    extraHold = extraHold,
    hasMbist = false,
    explictBist = hasMbist,
    explicitHold = false,
    suffix = suffix,
    powerCtl = powerCtl,
    foundry = foundry,
    sramInst = sramInst
  ))

  private val pip = if(isc > 1) 1 else 0
  private val mbp = Ram2MbistParams(sp, set, singlePort = true, ram.sramName, "", foundry, sramInst, pip, "None", () => this.pathName)

  val io = IO(new Bundle {
    val wreq  = Flipped(Decoupled(new DpSramWrite(gen, set, way)))
    val rreq  = Flipped(Decoupled(UInt(log2Ceil(set).W)))
    val rresp = Valid(Vec(way, gen))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })
  private val dataReg     = Option.when(outputReg)(RegEnable(ram.io.r.resp.data, ram.io.r.resp.valid))
  private val validReg    = Option.when(outputReg)(RegNext(ram.io.r.resp.valid, false.B))
  private val mbistMerged = genMbistBoreSink(mbp, hasMbist, een = false)
  private val finalWreq   = Wire(Decoupled(new DpSramWrite(UInt(sp.sramSegBits.W), set, sp.sramMaskBits)))
  private val finalRreq   = Wire(Decoupled(UInt(log2Ceil(set).W)))
  finalWreq.valid     := io.wreq.valid
  finalWreq.bits.addr := io.wreq.bits.addr
  finalWreq.bits.mask.foreach(_ := sp.funcMaskConverse(io.wreq.bits.mask.getOrElse(Fill(way, true.B))))
  finalWreq.bits.data := io.wreq.bits.data.asTypeOf(finalWreq.bits.data)
  finalRreq.valid     := io.rreq.valid
  finalRreq.bits      := io.rreq.bits
  io.wreq.ready       := finalWreq.ready
  io.rreq.ready       := finalRreq.ready

  if(hasMbist) {
    ram.io.broadcast.get := mbistMerged.broadcast
    when(mbistMerged.ack) {
      finalWreq.valid     := mbistMerged.we
      finalWreq.bits.addr := mbistMerged.addr
      finalWreq.bits.mask.foreach(_ := mbistMerged.wmask)
      finalWreq.bits.data := mbistMerged.wdata.asTypeOf(finalWreq.bits.data)
      finalRreq.valid     := mbistMerged.re
      finalRreq.bits      := mbistMerged.addr_rd
    }
    mbistMerged.rdata := io.rresp.bits.asUInt
  }

  ram.io.pwctl.foreach(pc => {
    pc.deact := Mux(mbistMerged.ack, false.B, io.pwctl.get.deact)
    pc.ret   := Mux(mbistMerged.ack, false.B, io.pwctl.get.ret)
    pc.stop  := Mux(mbistMerged.ack, false.B, io.pwctl.get.stop)
  })
  io.rresp.valid := validReg.getOrElse(ram.io.r.resp.valid)
  io.rresp.bits  := dataReg.getOrElse(ram.io.r.resp.data).asTypeOf(io.rresp.bits)

  ram.io.r.req.valid := finalRreq.valid
  ram.io.w.req.valid := finalWreq.valid
  finalRreq.ready    := ram.io.r.req.ready
  finalWreq.ready    := ram.io.w.req.ready

  private val wreqReg = if(isc > 1) RegEnable(finalWreq.bits, finalWreq.fire) else finalWreq.bits
  private val rreqReg = if(isc > 1) RegEnable(finalRreq.bits, finalRreq.fire) else finalRreq.bits
  ram.io.r.req.bits.setIdx := rreqReg
  ram.io.w.req.bits.setIdx := wreqReg.addr
  ram.io.w.req.bits.data   := wreqReg.data
  ram.io.w.req.bits.waymask.foreach(_ := wreqReg.mask.get)
}

class BankedDualPortSramTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  bank: Int = 1,
  shouldReset: Boolean = false,
  holdRead: Boolean = false,
  bypassWrite: Boolean = false,
  setup: Int = 1,
  latency: Int = 1,
  extraHold: Boolean = false,
  hasMbist: Boolean = false,
  suffix: String = "",
  powerCtl: Boolean = false,
  outputReg: Boolean = false,
  foundry: String = "Unknown",
  sramInst: String = "STANDARD",
  moduleName: Option[String] = None
) extends Module {

  val io = IO(new Bundle {
    val wreq  = Flipped(Decoupled(new DpSramWrite(gen, set, way)))
    val rreq  = Flipped(Decoupled(UInt(log2Ceil(set).W)))
    val rresp = Valid(Vec(way, gen))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })

  require(set % bank == 0)
  require(bank == (1 << log2Ceil(bank)))
  private val bset  = set / bank
  private val bselb = log2Ceil(bank)
  private val banks = Seq.fill(bank)(Module(new DualPortSramTemplate(
    gen = gen,
    set = bset,
    way = way,
    shouldReset = shouldReset,
    holdRead = holdRead,
    bypassWrite = bypassWrite,
    setup = setup,
    latency = latency,
    extraHold = extraHold,
    hasMbist = hasMbist,
    suffix = suffix,
    powerCtl = powerCtl,
    outputReg = outputReg,
    foundry = foundry,
    sramInst = sramInst,
    moduleName = moduleName
  )))

  if(bank == 1) {
    banks.head.io <> io
  } else {
    val rreqRdyVec  = Wire(Vec(bank, Bool()))
    val wreqRdyVec  = Wire(Vec(bank, Bool()))
    val rrespVldVec = Wire(Vec(bank, Bool()))
    val rrespDatVec = Wire(Vec(bank, Vec(way, gen)))
    val rbsel       = io.rreq.bits(bselb - 1, 0)
    val wbsel       = io.wreq.bits.addr(bselb - 1, 0)
    val respVld     = rrespVldVec.asUInt.orR
    val rbsel1hReg  = RegEnable(rrespVldVec.asUInt, respVld)
    val respSel1h   = Mux(respVld, rrespVldVec.asUInt, rbsel1hReg)
    for(i <- banks.indices) {
      val rhit = rbsel === i.U
      banks(i).io.rreq.valid := io.rreq.valid && rhit
      rreqRdyVec(i)          := banks(i).io.rreq.ready && rhit
      banks(i).io.rreq.bits  := io.rreq.bits >> bselb

      val whit = wbsel === i.U
      banks(i).io.wreq.valid     := io.wreq.valid && whit
      wreqRdyVec(i)              := banks(i).io.wreq.ready && whit
      banks(i).io.wreq.bits      := io.wreq.bits
      banks(i).io.wreq.bits.addr := io.wreq.bits.addr >> bselb

      rrespVldVec(i) := banks(i).io.rresp.valid
      rrespDatVec(i) := banks(i).io.rresp.bits

      banks(i).io.pwctl.foreach(_ := io.pwctl.get)
    }
    io.rreq.ready  := rreqRdyVec.asUInt.orR
    io.wreq.ready  := wreqRdyVec.asUInt.orR
    io.rresp.valid := respVld
    io.rresp.bits := Mux1H(respSel1h, rrespDatVec)
  }
}
