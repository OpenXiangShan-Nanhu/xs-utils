package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{ArbiterIO, Decoupled, MixedVec, Valid, log2Ceil}
import circt.stage.ChiselStage
import xs.utils.GlobalData
import xs.utils.test.WrapperTestPayload
import xs.utils.verilog._

import scala.util.Random

private[test] case class LatchFastCase(n: Int, width: Int = 17, shape: String = "bundle", factory: Boolean = false, compatibility: Boolean = false)

private[test] class LatchFastPort(c: LatchFastCase) extends Bundle {
  val in = Input(Vec(c.n, Valid(WrapperTestPayload(c.width, c.shape))))
  val out = Decoupled(WrapperTestPayload(c.width, c.shape))
  val ready = Output(UInt(c.n.W))
  val chosen = Output(UInt(math.max(1, log2Ceil(c.n)).W))
}

private[test] class LatchFastBank(cases: Seq[LatchFastCase]) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(MixedVec(cases.map(new LatchFastPort(_))))
  cases.zip(io).foreach { case (c, port) =>
    val gen = chiselTypeOf(port.in.head.bits)
    val actual = if (c.compatibility) Module(new xs.utils.LatchFastArbiter(gen, c.n)).io
      else if (c.factory) SvLatchFastArbiter(gen, c.n) else Module(new SvLatchFastArbiter(gen, c.n)).io
    actual.in.zip(port.in).foreach { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    port.out <> actual.out
    port.ready := VecInit(actual.in.map(_.ready)).asUInt
    port.chosen := actual.chosen
  }
}

private[test] class LatchFastOnly(n: Int) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(new ArbiterIO(UInt(17.W), n))
  private val arbiter = Module(new SvLatchFastArbiter(UInt(17.W), n))
  require(arbiter.core.isDefined == (n > 1))
  io <> arbiter.io
}

private[test] class LatchFastFactories(n: Int) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(new Bundle {
    val in = Input(Vec(n, Valid(WrapperTestPayload(66, "vecBundle"))))
    val out = Vec(4, Decoupled(WrapperTestPayload(66, "vecBundle")))
    val ready = Output(Vec(4, UInt(n.W)))
  })
  for (api <- 0 until 4) {
    val inputs = Wire(Vec(n, Decoupled(chiselTypeOf(io.in.head.bits))))
    inputs.zip(io.in).foreach { case (sink, source) => sink.valid := source.valid; sink.bits := source.bits }
    api match {
      case 0 => io.out(api) <> SvLatchFastArbiter(inputs.toSeq)
      case 1 => io.out(api) <> SvLatchFastArbiter(inputs)
      case 2 => SvLatchFastArbiter(inputs.toSeq, io.out(api))
      case 3 => SvLatchFastArbiter(inputs.toSeq, io.out(api), name = Some("latched"))
    }
    io.ready(api) := VecInit(inputs.map(_.ready)).asUInt
  }
}

class SvLatchFastArbiterSpec extends SimTestSpec {
  private class Model(n: Int) {
    var occupied = false
    var payload = BigInt(0)
    var chosen = 0
    private var waiting = Set.empty[Int]
    private var last = -1
    def reset(): Unit = {
      occupied = false; payload = 0; chosen = 0; waiting = Set.empty; last = -1
    }
    def valid: Boolean = occupied
    private def candidate(mask: BigInt): Int =
      waiting.toSeq.sorted.find(_ > last).filter(mask.testBit)
        .orElse((0 until n).find(mask.testBit)).get
    def readyMask(mask: BigInt): BigInt =
      if (occupied) BigInt(0)
      else if (n == 1) BigInt(1)
      else if (mask == 0) BigInt(0)
      else BigInt(1) << candidate(mask)
    def step(mask: BigInt, ready: Boolean, bits: Seq[BigInt]): Unit = {
      if (!occupied && mask != 0) {
        chosen = candidate(mask)
        occupied = true
        payload = bits(chosen)
        waiting = (0 until n).filter(i => mask.testBit(i) && i != chosen).toSet
        last = chosen
      } else if (occupied && ready) occupied = false
    }
  }

  private def drive(port: LatchFastPort, mask: BigInt, ready: Boolean, bits: Seq[BigInt]): Unit = {
    port.in.zipWithIndex.foreach { case (lane, i) => lane.valid.poke(mask.testBit(i).B); pokePayload(lane.bits, bits(i)) }
    port.out.ready.poke(ready.B)
  }

  private def check(port: LatchFastPort, model: Model, mask: BigInt, ready: Boolean, clue: String): Unit = {
    port.out.valid.expect(model.valid.B, clue)
    if (model.occupied) {
      expectPayload(port.out.bits, model.payload, clue)
      port.chosen.expect(model.chosen.U, clue)
    }
    port.ready.expect(model.readyMask(mask).U, clue)
  }

  describe("SvLatchFastArbiter") {
    it("acknowledges enqueue, holds payload and choice independently of inputs, and resets without a clock") {
      val c = LatchFastCase(5, 66, "vecBundle")
      val model = new Model(c.n)
      simulate(new LatchFastBank(Seq(c))) { dut =>
        val port = dut.io.head
        var serial = 0
        def cycle(mask: Int, ready: Boolean): Unit = {
          val bits = Seq.tabulate(c.n)(i => (BigInt(serial + 1) << 58) | BigInt(i + 1))
          drive(port, mask, ready, bits)
          check(port, model, mask, ready, s"cycle=$serial mask=$mask")
          dut.clock.step()
          model.step(mask, ready, bits)
          serial += 1
        }
        dut.reset.poke(true.B)
        drive(port, 0, false, Seq.fill(c.n)(BigInt(0)))
        dut.clock.step()
        dut.reset.poke(false.B)
        cycle(0x19, ready = false) // Enqueue lane 0 and acknowledge its producer.
        val captured = model.payload
        cycle(0x1f, ready = false) // New input data cannot change the stored output.
        assert(model.payload == captured)
        cycle(0x18, ready = false) // Withdraw lane 0; queued output stays valid.
        cycle(0, ready = false) // Even with no live inputs, retain the transaction.
        cycle(0, ready = true) // Consume the original lane 0 payload without reassertion.
        assert(!model.occupied)
        cycle(0x1f, ready = true) // Bubble: enqueue historical lane 3, not new lane 1.
        assert(model.occupied && model.chosen == 3)
        cycle(0x1f, ready = false)
        dut.reset.poke(true.B)
        model.reset()
        check(port, model, 0x1f, ready = false, "asynchronous reset")
        dut.reset.poke(false.B)
        cycle(0x1f, ready = true)
        assert(model.chosen == 0)
        cycle(0x1f, ready = true)
      }
    }

    it("matches an independent model cycle by cycle across widths, lanes and compatibility APIs") {
      val cases = Seq(LatchFastCase(1, 1), LatchFastCase(2, factory = true), LatchFastCase(3),
        LatchFastCase(5, 66, "vecBundle", factory = true), LatchFastCase(8, 129, "vecBundle"),
        LatchFastCase(33, factory = true), LatchFastCase(65, compatibility = true))
      val models = cases.map(c => new Model(c.n))
      val random = new Random(testSeed(0x1a7cL))
      simulate(new LatchFastBank(cases)) { dut =>
        for (cycle <- 0 until testCycles(1000)) {
          val reset = cycle == 0 || cycle % 79 == 0
          dut.reset.poke(reset.B)
          for (((c, port), model) <- cases.zip(dut.io).zip(models)) {
            if (reset) model.reset()
            val mask = if (cycle % 29 == 0) BigInt(0)
              else if (cycle % 7 == 0) (BigInt(1) << c.n) - 1 else BigInt(c.n, random)
            val ready = random.nextBoolean()
            val bits = Seq.fill(c.n)(BigInt(c.width, random))
            drive(port, mask, ready, bits)
            check(port, model, mask, ready, s"$c cycle=$cycle")
            if (!reset) model.step(mask, ready, bits)
          }
          dut.clock.step()
        }
      }
    }

    it("keeps capture and transfer on separate cycles, including a single input") {
      val cases = Seq(1, 2, 3, 5, 33, 65).map(LatchFastCase(_))
      val models = cases.map(c => new Model(c.n))
      simulate(new LatchFastBank(cases)) { dut =>
        dut.reset.poke(true.B)
        cases.zip(dut.io).foreach { case (c, port) => drive(port, 0, false, Seq.fill(c.n)(BigInt(0))) }
        dut.clock.step()
        dut.reset.poke(false.B)
        for (cycle <- 0 until 260) {
          for (((c, port), model) <- cases.zip(dut.io).zip(models)) {
            val mask = (BigInt(1) << c.n) - 1
            val bits = Seq.tabulate(c.n)(i => BigInt(cycle * 128 + i))
            drive(port, mask, true, bits)
            check(port, model, mask, true, s"n=${c.n} cycle=$cycle")
            port.out.valid.expect((cycle % 2 == 1).B)
            if (cycle % 2 == 1) port.chosen.expect(((cycle / 2) % c.n).U)
            model.step(mask, true, bits)
          }
          dut.clock.step()
        }
      }
    }

    it("keeps all sequence and output/name factories latched even for one input") {
      for (n <- Seq(1, 5)) {
        val random = new Random(testSeed())
        val model = new Model(n)
        simulate(new LatchFastFactories(n)) { dut =>
          for (cycle <- 0 until testCycles(150)) {
            val reset = cycle == 0 || cycle % 53 == 0
            dut.reset.poke(reset.B)
            if (reset) model.reset()
            val mask = BigInt(n, random)
            val ready = random.nextBoolean()
            val bits = Seq.fill(n)(BigInt(66, random))
            dut.io.in.zipWithIndex.foreach { case (lane, i) => lane.valid.poke(mask.testBit(i).B); pokePayload(lane.bits, bits(i)) }
            dut.io.out.zipWithIndex.foreach { case (out, api) =>
              out.ready.poke(ready.B)
              out.valid.expect(model.valid.B, s"n=$n api=$api cycle=$cycle")
              if (model.occupied) expectPayload(out.bits, model.payload)
              dut.io.ready(api).expect(model.readyMask(mask).U)
            }
            if (!reset) model.step(mask, ready, bits)
            dut.clock.step()
          }
        }
      }
    }

    it("reuses the depth-one queue without adding an arbiter SV module or duplicate dependencies") {
      GlobalData.synchronized {
        val saved = GlobalData.prefix
        try for (prefix <- Seq("", "soc_"); n <- Seq(1, 3)) {
          GlobalData.prefix = prefix
          val rtl = ChiselStage.emitSystemVerilog(new LatchFastOnly(n))
          val modules = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
          val registry = new VlogRegistry()
          val arbitration = if (n == 1) Seq.empty else registry.sources(SvFastArbiterImpl)
          val dependencies = (arbitration ++ registry.sources(SvFastQueueD1Impl)).map(_.moduleName).distinct
          assert(modules.sorted == (dependencies :+ "LatchFastOnly").sorted)
          assert(!modules.exists(_.contains("SvLatchFastArbiter")))
          assert(rtl.contains("posedge reset"))
        } finally GlobalData.prefix = saved
      }
      val chirrtl = ChiselStage.emitCHIRRTL(new LatchFastFactories(3))
      assert(chirrtl.contains("inst latched_arb of"))
    }

    it("supports defaults and singleton queues while rejecting invalid configurations") {
      ChiselStage.emitCHIRRTL(new Module {
        override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
        val defaults: ArbiterIO[UInt] = SvLatchFastArbiter()
        assert(defaults.in.size == 2 && defaults.out.bits.getWidth == 32)
        val singleton = Module(new SvLatchFastArbiter(UInt(13.W), 1))
        singleton.io.in.head.valid := true.B
        singleton.io.in.head.bits := 7.U(13.W)
        singleton.io.out.ready := true.B
      })
      for (n <- Seq(0, -1)) intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = Module(new SvLatchFastArbiter(UInt(8.W), n)) })
      }
      for (n <- Seq(1, 2); gen <- Seq(UInt(), UInt(0.W))) intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = SvLatchFastArbiter(gen, n) })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = SvLatchFastArbiter(Seq.empty[chisel3.util.DecoupledIO[UInt]]) })
      }
      intercept[chisel3.ExpectedHardwareException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = SvLatchFastArbiter(Seq(Decoupled(UInt(8.W)))) })
      }
    }
  }
}
