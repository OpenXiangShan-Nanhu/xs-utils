package xs.utils.sram

import chisel3.Data
import xs.utils.ClockGate
import xs.utils.constr._

import scala.collection.mutable

case class SramTimingMeta[T <: Data](
  iSetup:Int = 1,
  iHold:Int = 0,
  oSetup:Int = 1,
  sp:Boolean = true,
  name:String = "",
  inst:SRAMTemplate[T] = null
) {
  val oHold = oSetup - 1
  val intvl = iSetup.max(oSetup).max(iHold + 1)

  def constr():Seq[ConstrStmt] = {
    val strQueue = new mutable.Queue[ConstrStmt]()
    def ramPath   = s"${ConstrManager.getPath(inst)}"
    def cgPath()  = s"${ConstrManager.getPath(inst)}/icg/CG"
    def arrPath() = s"$ramPath/array"
    if(intvl > 1) {
      strQueue.addOne(AnyConstr(s"# $name"))
      strQueue.addOne(CreateGeneratedClock(
        target = ClockGate.clockGateCellInfo.clockOutPin(cgPath),
        source = ClockGate.clockGateCellInfo.clockInPin(cgPath),
        divide = Some(intvl),
        duty = Some(1.0 / (intvl * 2)),
      ))
    }

    if(sp) {
      if(iSetup > 1 || iHold > 0) {
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, to = Some(SramConstr.spSramInfo.inputPins(arrPath))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, to = Some(SramConstr.spSramInfo.inputPins(arrPath))))
      }
      if(oSetup > 1) {
        strQueue.addOne(Multicycle(oSetup, hold = false, start = false, end = true, from = Some(SramConstr.spSramInfo.clockPins(arrPath)), through = Some(SramConstr.spSramInfo.rdataPins(arrPath))))
        strQueue.addOne(Multicycle(oHold,  hold = true,  start = false, end = true, from = Some(SramConstr.spSramInfo.clockPins(arrPath)), through = Some(SramConstr.spSramInfo.rdataPins(arrPath))))
      }
    } else {
      if(iSetup > 1 || iHold > 0) {
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, to = Some(SramConstr.dpSramInfo.inputPins(arrPath))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, to = Some(SramConstr.dpSramInfo.inputPins(arrPath))))
      }
      if(oSetup > 1) {
        strQueue.addOne(Multicycle(oSetup, hold = false, start = false, end = true, from = Some(SramConstr.dpSramInfo.clockPins(arrPath)), through = Some(SramConstr.dpSramInfo.rdataPins(arrPath))))
        strQueue.addOne(Multicycle(oHold,  hold = true,  start = false, end = true, from = Some(SramConstr.dpSramInfo.clockPins(arrPath)), through = Some(SramConstr.dpSramInfo.rdataPins(arrPath))))
      }
    }
    if(strQueue.nonEmpty) strQueue.addOne(AnyConstr(""))
    strQueue.toSeq
  }
}

case class SramCellInfo(
  instPattern:String,
  clockPort:String,
  inputPorts:Seq[String],
  outputPort:String
) {
  def inputPins(wrapper:() => String):GetPins = {
    GetPins(
      target = () => "",
      of = Some(GetCells(() => s"${wrapper()}/$instPattern")),
      filter = Some(inputPorts.map(p => s"(full_name =~ */$p)").mkString("\"", "||", "\""))
    )
  }
  def rdataPins(wrapper:() => String):GetPins = {
    GetPins(
      target = () => "",
      of = Some(GetCells(() => s"${wrapper()}/$instPattern")),
      filter = Some(s"\"full_name =~ */$outputPort\"")
    )
  }
  def clockPins(wrapper:() => String):GetPins = {
    GetPins(
      target = () => "",
      of = Some(GetCells(() => s"${wrapper()}/$instPattern")),
      filter = Some(s"\"full_name =~ */$clockPort\"")
    )
  }
}

object SramConstr {
  var spSramInfo = SramCellInfo(
    instPattern = "mem*",
    clockPort = "RW0_clk",
    inputPorts = Seq("RW0_addr*", "RW0_en*", "RW0_wmode*", "RW0_wdata*", "RW0_wmode*", "RW0_wmask*"),
    outputPort = "RW0_rdata*"
  )
  var dpSramInfo = SramCellInfo(
    instPattern = "mem*",
    clockPort = "RW0_clk",
    inputPorts = Seq("R0_en*", "R0_addr*", "R0_data*", "W0_en*", "W0_addr*", "W0_data*", "W0_mask*"),
    outputPort = "RW0_rdata*"
  )
  private val ramInstPool = new mutable.Queue[SramTimingMeta[_ <: Data]]

  def registerRamInst[T <: Data](meta: SramTimingMeta[T]):Unit = {
    ramInstPool.addOne(meta)
  }

  def exportTimingConstr(eval:Boolean = false):Unit = {
    ramInstPool.flatMap(m => m.constr()).toSeq.foreach(ConstrManager.registerConstr)
    ramInstPool.clear()
    if(eval) ConstrManager.evalConstr()
  }
}
