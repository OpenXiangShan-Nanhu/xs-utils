package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{ArbiterIO, Cat, Decoupled, DecoupledIO, Mux1H, OHToUInt, RegEnable, Valid, MixedVec, log2Ceil}
import circt.stage.ChiselStage
import xs.utils.GlobalData
import xs.utils.test.WrapperTestPayload
import xs.utils.verilog._

import scala.util.Random

// Frozen pre-SV FastArbiter implementation for cycle-by-cycle equivalence tests.
private[test] class LegacyFastArbiter[T <: Data](gen: T, n: Int) extends Module {
  val io = IO(new ArbiterIO(gen, n))
  private def maskToOH(seq: Seq[Bool]): Seq[Bool] = seq.zipWithIndex.map {
    case (b, 0) => b
    case (b, i) => b && !Cat(seq.take(i)).orR
  }

  val chosenOH = Wire(UInt(n.W))
  val valids = VecInit(io.in.map(_.valid)).asUInt
  // Requests not selected at the last successful transfer.
  val pendingMask = RegEnable(
    valids & (~chosenOH).asUInt,
    0.U(n.W),
    io.out.fire
  )
  // Keep only indices strictly higher than the last chosen lane.
  val rrGrantMask = RegEnable(VecInit((0 until n) map { i =>
    if (i == 0) false.B else chosenOH(i - 1, 0).orR
  }).asUInt, 0.U(n.W), io.out.fire)
  val rrSelOH = VecInit(maskToOH((rrGrantMask & pendingMask).asBools)).asUInt
  val firstOneOH = VecInit(maskToOH(valids.asBools)).asUInt
  val rrValid = (rrSelOH & valids).orR
  chosenOH := Mux(rrValid, rrSelOH, firstOneOH)

  io.out.valid := valids.orR
  io.out.bits := Mux1H(chosenOH, io.in.map(_.bits))

  io.in.map(_.ready).zip(chosenOH.asBools).foreach {
    case (rdy, grant) => rdy := grant && io.out.ready
  }

  io.chosen := OHToUInt(chosenOH)
}

private[test] case class FastCase(size: Int, width: Int = 17, shape: String = "bundle", api: String = "fast")

private[test] class FastPort(c: FastCase) extends Bundle {
  val in = Input(Vec(c.size, Valid(WrapperTestPayload(c.width, c.shape))))
  val out = Decoupled(WrapperTestPayload(c.width, c.shape))
  val ready = Output(UInt(c.size.W))
  val chosen = Output(UInt(math.max(1, log2Ceil(c.size)).W))
}

private[test] class FastBank(cases: Seq[FastCase]) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(MixedVec(cases.map(new FastPort(_))))
  cases.zip(io).foreach { case (c, port) =>
    val gen = chiselTypeOf(port.in.head.bits)
    val arb = c.api match {
      case "fastFactory" => SvFastArbiter(gen, c.size)
      case "alias" => Module(new xs.utils.FastArbiter(gen, c.size)).io
      case "aliasFactory" => xs.utils.FastArbiter(gen, c.size)
      case "reference" => Module(new LegacyFastArbiter(gen, c.size)).io
      case "core" =>
        val arb = Wire(new ArbiterIO(gen, c.size))
        val core = Module(new SvFastArbiterImpl(c.width, c.size))
        core.clock := clock
        core.reset := reset.asAsyncReset
        core.io.in.valid := VecInit(arb.in.map(_.valid)).asUInt
        core.io.in.bits := VecInit(arb.in.map(_.bits)).asUInt
        core.io.out.ready := arb.out.ready
        arb.out.valid := core.io.out.valid
        arb.out.bits := core.io.out.bits.asTypeOf(gen)
        arb.chosen := core.io.chosen
        arb.in.zip(core.io.in.ready.asBools).foreach { case (lane, ready) => lane.ready := ready }
        arb
      case "fast" => Module(new SvFastArbiter(gen, c.size)).io
      case api => throw new IllegalArgumentException(s"Unknown FAST test API: $api")
    }
    arb.in.zip(port.in).foreach { case (sink, in) => sink.valid := in.valid; sink.bits := in.bits }
    port.out <> arb.out
    port.ready := VecInit(arb.in.map(_.ready)).asUInt
    port.chosen := arb.chosen
  }
}

private[test] class FastMixedBank(fastFirst: Boolean) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(Vec(2, new ArbiterIO(UInt(17.W), 3)))
  private val arbiters = if (fastFirst)
    Seq(Module(new SvFastArbiter(UInt(17.W), 3)).io, Module(new SvVipArbiter(UInt(17.W), 3)).io)
  else Seq(Module(new SvVipArbiter(UInt(17.W), 3)).io, Module(new SvFastArbiter(UInt(17.W), 3)).io)
  io.zip(arbiters).foreach { case (port, arb) => port <> arb }
}

private[test] class FastArbiterFactories(size: Int) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(new Bundle {
    val in = Input(Vec(size, Valid(WrapperTestPayload(66, "vecBundle"))))
    val out = Vec(8, Decoupled(WrapperTestPayload(66, "vecBundle")))
    val ready = Output(Vec(8, UInt(size.W)))
  })
  for (api <- 0 until 8) {
    val inputs = Wire(Vec(size, Decoupled(chiselTypeOf(io.in.head.bits))))
    inputs.zip(io.in).foreach { case (sink, in) => sink.valid := in.valid; sink.bits := in.bits }
    api match {
      case 0 => io.out(api) <> SvFastArbiter(inputs.toSeq)
      case 1 => io.out(api) <> SvFastArbiter(inputs)
      case 2 => io.out(api) <> xs.utils.FastArbiter(inputs.toSeq)
      case 3 => io.out(api) <> xs.utils.FastArbiter(inputs)
      case 4 => SvFastArbiter(inputs.toSeq, io.out(api))
      case 5 => SvFastArbiter(inputs.toSeq, io.out(api), name = Some("fast"))
      case 6 => xs.utils.FastArbiter(inputs.toSeq, io.out(api))
      case 7 => xs.utils.FastArbiter(inputs.toSeq, io.out(api), name = Some("legacy"))
    }
    io.ready(api) := VecInit(inputs.map(_.ready)).asUInt
  }
}

class SvFastArbiterSpec extends SimTestSpec {
  private def drive(port: FastPort, valid: BigInt, ready: Boolean, bits: Seq[BigInt]): Unit = {
    port.in.zipWithIndex.foreach { case (lane, i) =>
      lane.valid.poke(valid.testBit(i).B)
      pokePayload(lane.bits, bits(i))
    }
    port.out.ready.poke(ready.B)
  }

  private def check(port: FastPort, selected: Option[Int], ready: Boolean, bits: Seq[BigInt], clue: String,
    bypass: Boolean = true): Unit = {
    if (port.in.size == 1 && bypass) {
      port.out.valid.expect(port.in.head.valid.peek(), clue)
      port.chosen.expect(0.U, clue)
      port.ready.expect((if (ready) 1 else 0).U, clue)
      expectPayload(port.out.bits, bits.head, clue)
    } else {
      port.out.valid.expect(selected.nonEmpty.B, clue)
      port.chosen.expect(selected.getOrElse(0).U, clue)
      port.ready.expect(selected.filter(_ => ready).map(i => BigInt(1) << i).getOrElse(BigInt(0)).U, clue)
      expectPayload(port.out.bits, selected.map(bits).getOrElse(BigInt(0)), clue)
    }
  }

  // Independent state model: save requesting indices and the last transfer.
  // A withdrawn first historical candidate causes fallback, not another historical search.
  private class Model(n: Int) {
    private var waiting = Set.empty[Int]
    private var last = -1
    def reset(): Unit = { waiting = Set.empty; last = -1 }
    def choose(valid: BigInt): Option[Int] =
      waiting.toSeq.sorted.find(_ > last).filter(valid.testBit)
        .orElse((0 until n).find(valid.testBit))
    def transfer(valid: BigInt, chosen: Option[Int], ready: Boolean): Unit = {
      if (ready) chosen.foreach { lane =>
        waiting = (0 until n).filter(i => valid.testBit(i) && i != lane).toSet
        last = lane
      }
    }
  }

  describe("SvFastArbiter") {
    it("bypasses the SV core for one input across wrappers, factories and aliases") {
      val cases = Seq("fast", "fastFactory", "alias", "aliasFactory")
        .map(api => FastCase(1, 66, "vecBundle", api))
      val rtl = ChiselStage.emitSystemVerilog(new FastBank(cases))
      assert(!rtl.contains("module SvFastArbiter #("))
      simulate(new FastBank(cases)) { dut =>
        for (reset <- Seq(false, true); valid <- Seq(false, true); ready <- Seq(false, true)) {
          dut.reset.poke(reset.B)
          cases.zip(dut.io).zipWithIndex.foreach { case ((_, port), i) =>
            val bits = (BigInt(i + 1) << 60) | BigInt(0x12345)
            drive(port, if (valid) 1 else 0, ready, Seq(bits))
            check(port, if (valid) Some(0) else None, ready, Seq(bits), s"singleton $i")
          }
        }
      }
      ChiselStage.emitCHIRRTL(new Module {
        val in = IO(Flipped(Decoupled(UInt(13.W))))
        assert(SvFastArbiter(Seq(in)) eq in)
        assert(xs.utils.FastArbiter(Seq(in)) eq in)
      })
    }

    it("infers singleton payload widths and preserves the FastArbiter type and companion aliases") {
      assert(xs.utils.FastArbiter eq SvFastArbiter)
      ChiselStage.emitCHIRRTL(new Module {
        override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
        val alias: xs.utils.FastArbiter[UInt] = Module(new xs.utils.FastArbiter(UInt(), 1))
        val wrappers = Seq(alias, Module(new SvFastArbiter(UInt(), 1)))
        wrappers.foreach { arb =>
          arb.io.in.head.valid := true.B
          arb.io.in.head.bits := 7.U(13.W)
          arb.io.out.ready := true.B
        }
        val factory = SvFastArbiter(UInt(), n = 1)
        factory.in.head.valid := true.B
        factory.in.head.bits := 7.U(13.W)
        factory.out.ready := true.B
      })
      val chirrtl = ChiselStage.emitCHIRRTL(new FastArbiterFactories(3))
      assert(chirrtl.contains("inst legacy_arb of"))
    }

    it("selects immediately, holds history on stalls, falls back on withdrawal and resets asynchronously") {
      val cases = Seq("fast", "core", "alias").map(api => FastCase(5, 66, "vecBundle", api))
      simulate(new FastBank(cases)) { dut =>
        val bits = Seq.tabulate(5)(i => (BigInt(i + 1) << 60) | (i + 17))
        def cycle(mask: Int, ready: Boolean, selected: Option[Int]): Unit = {
          dut.io.foreach { port => drive(port, mask, ready, bits); check(port, selected, ready, bits, s"mask=$mask") }
          dut.clock.step()
        }
        dut.reset.poke(true.B)
        cycle(0, ready = false, None)
        dut.reset.poke(false.B)
        cycle(0x19, ready = true, Some(0)) // Save pending lanes 3 and 4.
        cycle(0x1f, ready = false, Some(3)) // Ignore newly arriving lower-index lanes.
        cycle(0x17, ready = false, Some(0)) // Lane 3 withdrew: do not skip to pending lane 4.
        cycle(0x1f, ready = false, Some(3)) // No handshake: history survives the withdrawal.
        cycle(0, ready = true, None)
        cycle(0x1f, ready = false, Some(3)) // Idle does not clear history either.
        dut.reset.poke(true.B) // No clock edge: the FAST core resets asynchronously.
        dut.io.foreach(port => check(port, Some(0), ready = false, bits, "async reset"))
        dut.reset.poke(false.B)
        cycle(0x1f, ready = true, Some(0))
        cycle(0x1f, ready = true, Some(1))
        cycle(0x1f, ready = true, Some(2))
        cycle(0x1f, ready = true, Some(3))
        cycle(0x1f, ready = true, Some(4))
        cycle(0x1f, ready = true, Some(0))
      }
    }

    it("exhausts historical and live request masks, including the raw single-input core") {
      val cases = Seq(1, 2, 3, 5).map(n => FastCase(n, api = "core"))
      simulate(new FastBank(cases)) { dut =>
        cases.zip(dut.io).foreach { case (c, port) => drive(port, 0, false, Seq.fill(c.size)(BigInt(0))) }
        for ((c, index) <- cases.zipWithIndex; previous <- 0 until (1 << c.size);
          valid <- 0 until (1 << c.size); ready <- Seq(false, true)) {
          val port = dut.io(index)
          val bits = Seq.tabulate(c.size)(i => BigInt(0x1111 * (i + 1)))
          val model = new Model(c.size)
          dut.reset.poke(true.B)
          dut.clock.step()
          dut.reset.poke(false.B)
          drive(port, previous, true, bits)
          val previousChoice = model.choose(previous)
          check(port, previousChoice, true, bits, "after reset", bypass = false)
          dut.clock.step()
          model.transfer(previous, previousChoice, ready = true)
          drive(port, valid, ready, bits)
          check(port, model.choose(valid), ready, bits, s"n=${c.size} previous=$previous valid=$valid", bypass = false)
        }
      }
    }

    it("matches the removed Chisel FastArbiter cycle by cycle with changing requests and backpressure") {
      val configs = Seq(
        FastCase(2, 1, api = "fast"), FastCase(3, api = "alias"),
        FastCase(5, 66, "vecBundle", "fastFactory"),
        FastCase(8, 129, "vecBundle", "aliasFactory"),
        FastCase(17, api = "fastFactory"), FastCase(32, api = "fast"),
        FastCase(33, api = "alias"), FastCase(65, api = "fast"))
      val cases = configs.flatMap(c => Seq(c, c.copy(api = "reference")))
      val models = configs.map(c => new Model(c.size))
      val random = new Random(testSeed())
      simulate(new FastBank(cases)) { dut =>
        for (cycle <- 0 until testCycles(1500)) {
          val reset = cycle == 0 || random.nextInt(89) == 0
          dut.reset.poke(reset.B)
          for (((c, model), index) <- configs.zip(models).zipWithIndex) {
            if (reset) model.reset()
            val valid = if (cycle % 31 == 0) BigInt(0)
              else if (cycle % 7 == 0) (BigInt(1) << c.size) - 1 else BigInt(c.size, random)
            val ready = random.nextBoolean()
            val bits = Seq.fill(c.size)(BigInt(c.width, random))
            val selected = model.choose(valid)
            for (offset <- 0 until 2) {
              val port = dut.io(2 * index + offset)
              drive(port, valid, ready, bits)
              check(port, selected, ready, bits, s"$c cycle=$cycle reference=${offset == 1}")
            }
            if (!reset) model.transfer(valid, selected, ready)
          }
          dut.clock.step()
        }
      }
    }

    it("sustains one transfer per cycle for persistent requests and singleton requesters") {
      val cases = Seq(2, 3, 5, 8, 17, 33, 65).map(n => FastCase(n, api = "fast"))
      simulate(new FastBank(cases)) { dut =>
        for (single <- Seq(false, true)) {
          dut.reset.poke(true.B)
          dut.clock.step()
          dut.reset.poke(false.B)
          for (cycle <- 0 until 100) {
            cases.zip(dut.io).foreach { case (c, port) =>
              val bits = Seq.tabulate(c.size)(i => BigInt(i + 1))
              val valid = if (single) BigInt(1) << (c.size - 1) else (BigInt(1) << c.size) - 1
              val chosen = if (single) c.size - 1 else cycle % c.size
              drive(port, valid, true, bits)
              check(port, Some(chosen), true, bits, s"n=${c.size} cycle=$cycle single=$single")
            }
            dut.clock.step()
          }
        }
      }
    }

    it("forwards FAST through every sequence factory and preserves legacy output/name overloads") {
      val random = new Random(testSeed())
      val model = new Model(5)
      simulate(new FastArbiterFactories(5)) { dut =>
        for (cycle <- 0 until testCycles(300)) {
          val reset = cycle == 0 || cycle % 67 == 0
          dut.reset.poke(reset.B)
          if (reset) model.reset()
          val valid = BigInt(5, random)
          val ready = random.nextBoolean()
          val bits = Seq.fill(5)(BigInt(66, random))
          dut.io.in.zipWithIndex.foreach { case (lane, i) =>
            lane.valid.poke(valid.testBit(i).B)
            pokePayload(lane.bits, bits(i))
          }
          val selected = model.choose(valid)
          dut.io.out.zipWithIndex.foreach { case (out, api) =>
            out.ready.poke(ready.B)
            out.valid.expect(selected.nonEmpty.B)
            expectPayload(out.bits, selected.map(bits).getOrElse(BigInt(0)), s"api=$api cycle=$cycle")
            dut.io.ready(api).expect(selected.filter(_ => ready).map(i => BigInt(1) << i).getOrElse(BigInt(0)).U)
          }
          if (!reset) model.transfer(valid, selected, ready)
          dut.clock.step()
        }
      }
    }

    it("emits an independent core and deduplicates helpers alongside VIP in either order") {
      GlobalData.synchronized {
        val saved = GlobalData.prefix
        try for (prefix <- Seq("", "soc_")) {
          GlobalData.prefix = prefix
          val registry = new VlogRegistry()
          val fastSources = registry.sources(SvFastArbiterImpl)
          val liveSources = registry.sources(SvLivelockFreeArbiterImpl)
          assert(fastSources.last.moduleName == prefix + "SvFastArbiter")
          assert(!fastSources.exists(_.moduleName.endsWith("SvLivelockFreeArbiter")))
          assert(!liveSources.exists(_.moduleName.endsWith("SvFastArbiter")))
          assert(!liveSources.last.text.contains("FAST"))
          assert(!liveSources.last.text.contains("pending_mask"))
          assert(!fastSources.last.text.contains("ROUND_ROBIN"))
          val fastRtl = ChiselStage.emitSystemVerilog(new FastBank(Seq(FastCase(2), FastCase(5))))
          val modules = "(?m)^\\s*module\\s+(\\w+)".r
          assert(modules.findAllMatchIn(fastRtl).map(_.group(1)).toSeq.sorted ==
            (fastSources.map(_.moduleName) :+ "FastBank").sorted)
          for (fastFirst <- Seq(false, true)) {
            val rtl = ChiselStage.emitSystemVerilog(new FastMixedBank(fastFirst))
            val expected = (fastSources ++ liveSources).map(_.moduleName).distinct :+ "FastMixedBank"
            assert(modules.findAllMatchIn(rtl).map(_.group(1)).toSeq.sorted == expected.sorted)
          }
        } finally GlobalData.prefix = saved
      }
    }

    it("rejects empty, non-hardware and invalid-width FastArbiter inputs") {
      for (n <- Seq(0, -1)) intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = Module(new xs.utils.FastArbiter(UInt(8.W), n)) })
      }
      for (gen <- Seq(UInt(), UInt(0.W))) intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = Module(new SvFastArbiter(gen, 2)) })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = SvFastArbiter(Seq.empty[DecoupledIO[UInt]]) })
      }
      intercept[chisel3.ExpectedHardwareException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = SvFastArbiter(Seq(Decoupled(UInt(8.W)))) })
      }
    }
  }
}
