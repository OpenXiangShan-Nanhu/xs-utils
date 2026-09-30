package xs.utils.constr

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
    ConstrManager.registerConstr(Multicycle(50, hold = false, end = true, from = Some(GetPorts(() => s"$port*"))))
    ConstrManager.registerConstr(Multicycle(49, hold = true,  end = true, from = Some(GetPorts(() => s"$port*"))))
  }
}

object McpReg {
  def apply(parent: chisel3.InstanceId, obj:String, value:Int):Unit = {
    ConstrManager.registerConstr(Multicycle(value,     hold = false, start = true, from = Some(GetPins(() => s"${ConstrManager.getPath(parent)}/$obj/CK"))))
    ConstrManager.registerConstr(Multicycle(value - 1, hold = true,  start = true, from = Some(GetPins(() => s"${ConstrManager.getPath(parent)}/$obj/CK"))))
  }
}
