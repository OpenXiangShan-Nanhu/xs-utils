package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.mbist.{Mbist, Ram2Mbist, Ram2MbistParams}
import xs.utils.sram.SramHelper.genMbistBoreSink

class SpSramReq[T <: Data](gen: T, set: Int, way: Int) extends Bundle {
  val addr  = UInt(log2Ceil(set).W)
  val mask  = if(way == 1) None else Some(UInt(way.W))
  val write = Bool()
  val data  = Vec(way, gen)
}

class SpSramResp[T <: Data](gen: T, way: Int) extends Bundle {
  val data = Vec(way, gen)
}

class SinglePortSramTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  shouldReset: Boolean = false,
  holdRead: Boolean = false,
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
  override val desiredName = moduleName.getOrElse("SinglePortSramTemplate")

  private val isc = if(extraHold) setup + 1 else setup
  private val sp  = SramInfo(gen.getWidth, way, bist = false)
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
    explicitHold = false,
    suffix = suffix,
    powerCtl = powerCtl,
    foundry = foundry,
    sramInst = sramInst
  ))

  private val pip = if(isc > 1) 1 else 0
  private val mbp = Ram2MbistParams(sp, set, singlePort = true, ram.sramName, "", foundry, sramInst, pip, "None", () => this.pathName)

  val io = IO(new Bundle {
    val req   = Flipped(Decoupled(new SpSramReq(gen, set, way)))
    val resp  = Valid(new SpSramResp(gen, way))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })
  private val dataReg     = Option.when(outputReg)(RegEnable(ram.io.r.resp.data, ram.io.r.resp.valid))
  private val validReg    = Option.when(outputReg)(RegNext(ram.io.r.resp.valid, false.B))
  private val finalReq    = Wire(Decoupled(new SpSramReq(UInt(sp.sramSegBits.W), set, sp.sramMaskBits)))
  private val mbistMerged = genMbistBoreSink(mbp, hasMbist, een = false)
  finalReq.valid      := io.req.valid
  finalReq.bits.addr  := io.req.bits.addr
  finalReq.bits.write := io.req.bits.write
  finalReq.bits.mask.foreach(_ := sp.funcMaskConverse(io.req.bits.mask.getOrElse(Fill(way, true.B))))
  finalReq.bits.data := io.req.bits.data.asTypeOf(finalReq.bits.data)
  io.req.ready       := finalReq.ready
  if(hasMbist) {
    ram.io.broadcast.get := mbistMerged.broadcast
    when(mbistMerged.ack) {
      finalReq.valid      := mbistMerged.we || mbistMerged.re
      finalReq.bits.addr  := mbistMerged.addr_rd
      finalReq.bits.write := mbistMerged.we
      finalReq.bits.mask.foreach(_ := mbistMerged.wmask)
      finalReq.bits.data := mbistMerged.wdata.asTypeOf(finalReq.bits.data)
    }
    mbistMerged.rdata := io.resp.bits.data.asUInt
  }

  ram.io.pwctl.foreach(pc => {
    pc.deact := Mux(mbistMerged.ack, false.B, io.pwctl.get.deact)
    pc.ret   := Mux(mbistMerged.ack, false.B, io.pwctl.get.ret)
    pc.stop  := Mux(mbistMerged.ack, false.B, io.pwctl.get.stop)
  })
  io.resp.valid     := validReg.getOrElse(ram.io.r.resp.valid)
  io.resp.bits.data := dataReg.getOrElse(ram.io.r.resp.data).asTypeOf(io.resp.bits.data)

  ram.io.r.req.valid := finalReq.valid && !finalReq.bits.write
  ram.io.w.req.valid := finalReq.valid && finalReq.bits.write
  finalReq.ready     := ram.io.w.req.ready

  private val addr = if(isc > 1) RegEnable(finalReq.bits.addr, finalReq.fire) else finalReq.bits.addr
  private val data = if(isc > 1) RegEnable(finalReq.bits.data, finalReq.fire && finalReq.bits.write) else finalReq.bits.data
  private val mask = finalReq.bits.mask.map(m => {
    if(isc > 1) {
      RegEnable(m, finalReq.fire && finalReq.bits.write)
    } else {
      m
    }
  })
  ram.io.r.req.bits.setIdx := addr
  ram.io.w.req.bits.setIdx := addr
  ram.io.w.req.bits.data   := data
  ram.io.w.req.bits.waymask.foreach(_ := mask.get)
}

class BankedSinglePortSramTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  bank: Int = 1,
  shouldReset: Boolean = false,
  holdRead: Boolean = false,
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
    val req   = Flipped(Decoupled(new SpSramReq(gen, set, way)))
    val resp  = Valid(new SpSramResp(gen, way))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })

  require(set % bank == 0)
  require(bank == (1 << log2Ceil(bank)))
  private val bset  = set / bank
  private val bselb = log2Ceil(bank)
  private val banks = Seq.fill(bank)(Module(new SinglePortSramTemplate(
    gen = gen,
    set = bset,
    way = way,
    shouldReset = shouldReset,
    holdRead = holdRead,
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
    val reqRdyVec  = Wire(Vec(bank, Bool()))
    val respVldVec = Wire(Vec(bank, Bool()))
    val respDatVec = Wire(Vec(bank, Vec(way, gen)))
    val bsel       = io.req.bits.addr(bselb - 1, 0)
    val respVld    = respVldVec.asUInt.orR
    val rbsel1hReg = RegEnable(respVldVec.asUInt, respVld)
    val respSel1h  = Mux(respVld, respVldVec.asUInt, rbsel1hReg)
    for(i <- banks.indices) {
      val hit = bsel === i.U
      banks(i).io.req.valid     := io.req.valid && hit
      reqRdyVec(i)              := banks(i).io.req.ready && hit
      banks(i).io.req.bits      := io.req.bits
      banks(i).io.req.bits.addr := io.req.bits.addr >> bselb

      respVldVec(i) := banks(i).io.resp.valid
      respDatVec(i) := banks(i).io.resp.bits
      banks(i).io.pwctl.foreach(_ := io.pwctl.get)
    }
    io.req.ready  := reqRdyVec.asUInt.orR
    io.resp.valid := respVldVec.asUInt.orR
    io.resp.bits  := Mux1H(respSel1h, respDatVec)
  }
}
