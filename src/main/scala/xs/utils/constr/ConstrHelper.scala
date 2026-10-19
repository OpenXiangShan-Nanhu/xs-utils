package xs.utils.constr

import chisel3.{ActualDirection, Bits, Data, Record, Vec}
import chisel3.reflect.DataMirror

object StaticReg {
  def apply(parent: chisel3.InstanceId, obj:String):Unit = {
    ConstrManager.registerConstr(Multicycle(50, hold = false, start = true, from = Some(GetPins(() => s"${ConstrManager.getPath(parent)}/$obj/CK"))))
    ConstrManager.registerConstr(Multicycle(49, hold = true,  start = true, from = Some(GetPins(() => s"${ConstrManager.getPath(parent)}/$obj/CK"))))
  }
}

object SynchronizerReg {
  def apply(parent: chisel3.InstanceId, obj:String):Unit = {
    ConstrManager.registerConstr(Multicycle(50, hold = false, end = true, to = Some(GetPins(() => s"${ConstrManager.getPath(parent)}/$obj/D"))))
    ConstrManager.registerConstr(Multicycle(49, hold = true,  end = true, to = Some(GetPins(() => s"${ConstrManager.getPath(parent)}/$obj/D"))))
  }
}

object StaticInputPort {
  def apply(port: String): Unit = {
    ConstrManager.registerConstr(Multicycle(50, hold = false, start = true, from = Some(GetPorts(() => s"$port*"))))
    ConstrManager.registerConstr(Multicycle(49, hold = true,  start = true, from = Some(GetPorts(() => s"$port*"))))
  }
}

object StaticInputBundle {
  private def isPureInput(data: Data): Boolean = data match {
    case bits: Bits => DataMirror.directionOf(bits) == ActualDirection.Input
    case record: Record => record.elements.nonEmpty && record.elements.values.forall(isPureInput)
    case vec: Vec[_] => vec.nonEmpty && vec.forall(isPureInput)
    case _ => false
  }

  /** Constrain input Bits in an IO Bundle/Record or Vec, using resolved directions.
    * `prefix` is the full flattened port prefix of `data`, e.g. "io_ctl" for io.ctl.
    * Pure input aggregates share one wildcard; mixed aggregates are traversed.
    * Outputs, empty aggregates and non-Bits leaves are skipped.
    */
  def apply(prefix: String, data: Data): Unit = {
    chisel3.experimental.requireIsHardware(data, "StaticInputBundle requires bound hardware")
    if (isPureInput(data)) {
      StaticInputPort(prefix)
    } else {
      data match {
        case record: Record =>
          record.elements.foreach { case (name, field) => apply(s"${prefix}_${name}", field) }
        case vec: Vec[_] =>
          vec.zipWithIndex.foreach { case (field, index) => apply(s"${prefix}_${index}", field) }
        case _ => ()
      }
    }
  }
}

object McpReg {
  def apply(parent: chisel3.InstanceId, obj:String, value:Int):Unit = {
    ConstrManager.registerConstr(Multicycle(value,     hold = false, start = true, from = Some(GetPins(() => s"${ConstrManager.getPath(parent)}/$obj/CK"))))
    ConstrManager.registerConstr(Multicycle(value - 1, hold = true,  start = true, from = Some(GetPins(() => s"${ConstrManager.getPath(parent)}/$obj/CK"))))
  }
}
