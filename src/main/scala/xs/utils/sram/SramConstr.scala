package xs.utils.sram

import chisel3.Data
import xs.utils.constr._

import scala.collection.mutable

case class SramTimingMeta[T <: Data](
  iSetup:Int = 1,
  iHold:Int = 0,
  oSetup:Int = 1,
  sp:Boolean = true,
  mask: Boolean = false,
  name:String = "",
  inst:SRAMTemplate[T] = null
) {
  val oHold = oSetup - 1
  val intvl = iSetup.max(oSetup).max(iHold + 1)

  def constr():Seq[ConstrStmt] = {
    val strQueue = new mutable.Queue[ConstrStmt]()
    def ramPath = s"${ConstrManager.getPath(inst)}"
    def cgPath  = s"${ConstrManager.getPath(inst)}/icg/CG"
    if(intvl > 1) {
      strQueue.addOne(AnyConstr(s"# $name"))
      strQueue.addOne(CreateGeneratedClock(
        target = GetPins(() => s"$cgPath/Q"),
        source = GetPins(() => s"$cgPath/CK"),
        divide = Some(intvl),
        duty = Some(1.0 / (intvl * 2)),
      ))
    }

    if(iSetup > 1) {
      strQueue.addOne(Multicycle(iSetup, hold = false, start = true, from = Some(GetPins(() => s"$ramPath/activeReg_0*/CK"))))
      strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, from = Some(GetPins(() => s"$ramPath/activeReg_0*/CK"))))
    }

    if(sp) {
      if(iSetup > 1 || iHold > 0) {
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_addr*"))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_addr*"))))
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_en*"))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_en*"))))
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_wmode*"))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_wmode*"))))
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_wdata*"))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_wdata*"))))
        if(mask) {
          strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_wmask*"))))
          strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/RW0_wmask*"))))
        }
      }
      if(oSetup > 1) {
        strQueue.addOne(Multicycle(oSetup, hold = false, start = false, end = true,  through = Some(GetPins(() => s"$ramPath/RW0_rdata*"))))
        strQueue.addOne(Multicycle(oHold,  hold = true,  start = false, end = true,  through = Some(GetPins(() => s"$ramPath/RW0_rdata*"))))
      }
    } else {
      if(iSetup > 1 || iHold > 0) {
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/R0_addr*"))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/R0_addr*"))))
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/W0_addr*"))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/W0_addr*"))))
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/W0_en*"))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/W0_en*"))))
        strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/W0_data*"))))
        strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/W0_data*"))))
        if(mask) {
          strQueue.addOne(Multicycle(iSetup, hold = false, start = true, through = Some(GetPins(() => s"$ramPath/array/W0_mask*"))))
          strQueue.addOne(Multicycle(iHold,  hold = true,  start = true, through = Some(GetPins(() => s"$ramPath/array/W0_mask*"))))
        }
      }
      if(oSetup > 1) {
        strQueue.addOne(Multicycle(oSetup, hold = false, start = false, end = true,  through = Some(GetPins(() => s"$ramPath/array/RW0_rdata*"))))
        strQueue.addOne(Multicycle(oHold,  hold = true,  start = false, end = true,  through = Some(GetPins(() => s"$ramPath/array/RW0_rdata*"))))
      }
    }
    if(strQueue.nonEmpty) strQueue.addOne(AnyConstr(""))
    strQueue.toSeq
  }
}

object SramConstr {
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
