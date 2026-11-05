package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.Decoupled
import circt.stage.ChiselStage
import xs.utils.test.WrapperTestPacket
import xs.utils.verilog.{SvFastQueue, SvMimoQueue}

private[test] class QueueInlineWrapperTop extends Module {
  val io = IO(new Bundle {
    val fastEnq = Flipped(Decoupled(new WrapperTestPacket(17)))
    val fastDeq = Decoupled(new WrapperTestPacket(17))
    val fastWaterline = Output(UInt(3.W))
    val fastCount = Output(UInt(2.W))
    val fastFree = Output(UInt(2.W))
    val mimoEnq = Vec(2, Flipped(Decoupled(Vec(3, new WrapperTestPacket(17)))))
    val mimoDeq = Vec(2, Decoupled(Vec(3, new WrapperTestPacket(17))))
    val mimoCount = Output(UInt(3.W))
  })

  private val fast = Module(new SvFastQueue(new WrapperTestPacket(17), size = 2))
  fast.io.enq.valid := io.fastEnq.valid
  fast.io.enq.bits := io.fastEnq.bits
  io.fastEnq.ready := fast.io.enq.ready
  io.fastDeq.valid := fast.io.deq.valid
  io.fastDeq.bits := fast.io.deq.bits
  fast.io.deq.ready := io.fastDeq.ready
  io.fastWaterline := fast.io.waterline
  io.fastCount := fast.io.count
  io.fastFree := fast.io.free

  private val mimo = Module(new SvMimoQueue(Vec(3, new WrapperTestPacket(17)), enqNum = 2, deqNum = 2, size = 4))
  mimo.io.enq.zip(io.mimoEnq).foreach { case (queue, port) =>
    queue.valid := port.valid
    queue.bits := port.bits
    port.ready := queue.ready
  }
  io.mimoDeq.zip(mimo.io.deq).foreach { case (port, queue) =>
    port.valid := queue.valid
    port.bits := queue.bits
    queue.ready := port.ready
  }
  io.mimoCount := mimo.io.count
}

private[test] class QueueSvDependencyTop extends Module {
  val io = IO(new Bundle {
    val fast = new xs.utils.verilog.SvFastQueuePort(8, 5)
    val mimo = new xs.utils.verilog.SvMimoQueuePort(8, 2, 3, 5)
  })
  private val fast = Module(new xs.utils.verilog.SvFastQueueDNImpl(8, 5))
  private val mimo = Module(new xs.utils.verilog.SvMimoQueueImpl(8, 2, 3, 5, false))
  fast.clock := clock
  fast.reset := reset
  mimo.clock := clock
  mimo.reset := reset
  io.fast <> fast.io
  io.mimo <> mimo.io
}

private[test] class QueueMixedSvDependencyTop extends QueueSvDependencyTop {
  val population = IO(Output(UInt(3.W)))
  val selected = IO(Output(UInt(8.W)))
  private val count = Module(new xs.utils.verilog.SvPopcountImpl(6))
  count.io_in := io.fast.waterline
  population := count.io_out
  private val mux = Module(new xs.utils.verilog.SvMux1HImpl(8, 2))
  mux.io_sel := io.mimo.enq.valid
  mux.io_in := io.mimo.enq.bits
  selected := mux.io_out
}

// This suite emits RTL so it also checks firtool's inlineInstance lowering, not just elaboration.
class QueueInlineSpec extends SimTestSpec {
  private def moduleNames(systemVerilog: String): Seq[String] =
    """(?m)^\s*module\s+([A-Za-z_][A-Za-z0-9_$]*)""".r
      .findAllMatchIn(systemVerilog).map(_.group(1)).toSeq

  private def hasInstanceOf(systemVerilog: String, moduleName: String): Boolean = {
    val typeName = java.util.regex.Pattern.quote(moduleName)
    s"""(?m)^\\s*$typeName\\s*(?:#\\s*\\(|\\w+\\s*\\()""".r.findFirstIn(systemVerilog).isDefined
  }

  describe("Queue Module wrappers") {
    it("preserves queue-package constructor types and factory singleton aliases") {
      implicitly[xs.utils.queue.FastQueue[WrapperTestPacket] =:= xs.utils.verilog.SvFastQueue[WrapperTestPacket]]
      implicitly[xs.utils.queue.MimoQueue[WrapperTestPacket] =:= xs.utils.verilog.SvMimoQueue[WrapperTestPacket]]
      implicitly[xs.utils.MimoQueue[WrapperTestPacket] =:= xs.utils.verilog.SvMimoQueue[WrapperTestPacket]]
      implicitly[xs.utils.queue.FastQueueFactory =:= xs.utils.verilog.SvFastQueueFactory]
      assert(xs.utils.queue.FastQueue eq xs.utils.verilog.SvFastQueue)
      assert(xs.utils.queue.FastQueueRaw eq xs.utils.verilog.SvFastQueueRaw)
      assert(xs.utils.queue.FastQueueNoX eq xs.utils.verilog.SvFastQueueNoX)
    }

    it("registers shared encoder, mux and population-count dependencies without Chisel instances") {
      val fir = ChiselStage.emitCHIRRTL(new QueueSvDependencyTop)
      val extmodules = "(?m)^\\s*extmodule\\s+(\\w+)".r.findAllMatchIn(fir).map(_.group(1)).toSeq
      assert(extmodules.toSet == Set("SvFastQueueDN", "SvMimoQueue"), extmodules.mkString(", "))
      val rtl = ChiselStage.emitSystemVerilog(new QueueSvDependencyTop)
      val top = rtl.substring(rtl.indexOf("module QueueSvDependencyTop")).split("endmodule")(0)
      for (dependency <- Seq("SvOHToUInt", "SvMux1H", "SvPopcount")) {
        assert(moduleNames(rtl).count(_ == dependency) == 1, s"expected one $dependency definition")
        assert(!hasInstanceOf(top, dependency), s"$dependency must not create a Chisel top-level instance")
      }
      val fast = rtl.substring(rtl.indexOf("module SvFastQueueDN #(")).split("endmodule")(0)
      assert(hasInstanceOf(fast, "SvOHToUInt"), "SvFastQueue must instantiate its SV encoder dependency")
      assert(fast.contains("u_count") && fast.contains("u_free"))
      val mimo = rtl.substring(rtl.indexOf("module SvMimoQueue #(")).split("endmodule")(0)
      assert(hasInstanceOf(mimo, "SvPopcount"), "SvMimoQueue must instantiate its SV population-count dependency")
      assert(mimo.contains("u_enq_fire_count") && mimo.contains("u_deq_fire_count"))
    }

    it("deduplicates helpers shared by direct ExtModules and SV-only dependencies") {
      val rtl = ChiselStage.emitSystemVerilog(new QueueMixedSvDependencyTop)
      val top = rtl.substring(rtl.indexOf("module QueueMixedSvDependencyTop")).split("endmodule")(0)
      for (name <- Seq("SvFastQueueDN", "SvMimoQueue", "SvMux1H", "SvPopcount")) {
        assert(moduleNames(rtl).count(_ == name) == 1, s"expected one $name definition")
        assert(hasInstanceOf(top, name), s"missing direct $name instance")
      }
    }

    it("simulates queues alongside directly instantiated shared helpers") {
      // This fixture exercises the raw ExtModule ports, not typed wrapper payloads.
      simulate(new QueueMixedSvDependencyTop) { dut =>
        dut.io.fast.enq.valid.poke(false.B)
        dut.io.fast.enq.bits.poke(0x3c.U)
        dut.io.fast.deq.ready.poke(false.B)
        dut.io.mimo.enq.valid.poke(0.U)
        dut.io.mimo.enq.bits.poke(0xa659.U)
        dut.io.mimo.deq.ready.poke(0.U)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        for ((mask, expected) <- Seq(0 -> 0, 1 -> 0x59, 2 -> 0xa6, 3 -> 0xff)) {
          dut.io.mimo.enq.valid.poke(mask.U)
          dut.selected.expect(expected.U)
          dut.population.expect(1.U) // SvFastQueue waterline is one-hot, even when empty.
        }
        dut.io.fast.enq.valid.poke(true.B)
        dut.io.mimo.enq.valid.poke(2.U)
        dut.clock.step()
        dut.io.fast.enq.valid.poke(false.B)
        dut.io.mimo.enq.valid.poke(0.U)
        dut.io.fast.deq.valid.expect(true.B)
        dut.io.fast.deq.bits.expect(0x3c.U)
        dut.io.mimo.count.expect(1.U)
        dut.io.mimo.deq.valid.expect(1.U)
        assert((dut.io.mimo.deq.bits.peek().litValue & 0xff) == 0xa6)
        dut.population.expect(1.U)
      }
    }

    it("inlines SvFastQueue and SvMimoQueue while retaining their ExtModule boundaries") {
      val systemVerilog = ChiselStage.emitSystemVerilog(new QueueInlineWrapperTop)
      val modules = moduleNames(systemVerilog)

      assert(modules.contains("QueueInlineWrapperTop"),
        s"Containing top is missing:\n$systemVerilog")

      assert(!modules.contains("SvFastQueue"),
        s"SvFastQueue wrapper module remained after inlining:\n$systemVerilog")
      assert(!hasInstanceOf(systemVerilog, "SvFastQueue"),
        s"SvFastQueue wrapper instance remained after inlining:\n$systemVerilog")
      assert(modules.contains("SvFastQueueD2"),
        s"SvFastQueueD2 ExtModule definition is missing:\n$systemVerilog")
      assert(hasInstanceOf(systemVerilog, "SvFastQueueD2"),
        s"SvFastQueueD2 ExtModule instance boundary is missing:\n$systemVerilog")

      // The SvMimoQueue wrapper and its ExtModule share the same desired module name.
      // If inlining failed, firtool would have to retain a second, renamed module.
      val mimoModules = modules.filter(_.matches("SvMimoQueue(?:_[0-9]+)?"))
      assert(mimoModules == Seq("SvMimoQueue"),
        s"Expected one SvMimoQueue ExtModule, found ${mimoModules.mkString(", ")}:\n$systemVerilog")
      assert(hasInstanceOf(systemVerilog, "SvMimoQueue"),
        s"SvMimoQueue ExtModule instance boundary is missing:\n$systemVerilog")
      assert(systemVerilog.contains("The core uses an explicit active-high asynchronous reset by convention."),
        s"SvMimoQueue ExtModule body is missing:\n$systemVerilog")
    }
  }
}
