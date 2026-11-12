package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{ArbiterIO, Decoupled, MixedVec, RRArbiter, Valid, log2Ceil}
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import xs.utils.GlobalData
import xs.utils.test.{WrapperTestPacket, WrapperTestPayload}
import xs.utils.verilog.{SvMux1HImpl, SvRRArbiter, VlogRegistry, SvLivelockFreeArbiterImpl}

import scala.util.Random

private[test] case class SvRRArbiterCase(
  width:   Int,
  n:       Int,
  payload: String = "bundle",
  factory: Boolean = false
)

private[test] class SvRRArbiterTestPort(c: SvRRArbiterCase) extends Bundle {
  val in = Input(Vec(c.n, Valid(WrapperTestPayload(c.width, c.payload))))
  val actualOut = Decoupled(WrapperTestPayload(c.width, c.payload))
  val referenceOut = Decoupled(WrapperTestPayload(c.width, c.payload))
  val actualInReady = Output(Vec(c.n, Bool()))
  val referenceInReady = Output(Vec(c.n, Bool()))
  val actualChosen = Output(UInt(math.max(1, log2Ceil(c.n)).W))
  val referenceChosen = Output(UInt(math.max(1, log2Ceil(c.n)).W))
}

private[test] class SvRRArbiterTestBank(cases: Seq[SvRRArbiterCase]) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous

  val io = IO(MixedVec(cases.map(new SvRRArbiterTestPort(_))))

  cases.zip(io).foreach { case (c, port) =>
    val payloadGen = chiselTypeOf(port.in.head.bits)
    val actualIO =
      if (c.factory) SvRRArbiter(payloadGen, c.n)
      else Module(new SvRRArbiter(payloadGen, c.n)).io
    val reference = withReset(reset.asAsyncReset) {
      Module(new RRArbiter(payloadGen, c.n, initLastGrant = true))
    }

    actualIO.in.zip(port.in).foreach { case (lane, input) =>
      lane.valid := input.valid
      lane.bits := input.bits
    }
    reference.io.in.zip(port.in).foreach { case (lane, input) =>
      lane.valid := input.valid
      lane.bits := input.bits
    }

    port.actualOut <> actualIO.out
    port.referenceOut <> reference.io.out
    port.actualInReady := VecInit(actualIO.in.map(_.ready))
    port.referenceInReady := VecInit(reference.io.in.map(_.ready))
    port.actualChosen := actualIO.chosen
    port.referenceChosen := reference.io.chosen
  }
}

private[test] class SvRRArbiterN1Port(gen: Data) extends Bundle {
  val in = Input(Valid(gen))
  val out = Decoupled(gen)
  val inReady = Output(Bool())
  val chosen = Output(UInt(1.W))
}

private[test] class SvRRArbiterN1WrapperTop(gen: Data) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(new SvRRArbiterN1Port(gen))

  private val arbiter = Module(new SvRRArbiter(gen, 1))
  arbiter.io.in.head.valid := io.in.valid
  arbiter.io.in.head.bits := io.in.bits
  io.inReady := arbiter.io.in.head.ready
  io.out <> arbiter.io.out
  io.chosen := arbiter.io.chosen
}

private[test] class SvRRArbiterN1FactoryTop(gen: Data) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(new SvRRArbiterN1Port(gen))

  private val arbiter = SvRRArbiter(gen, 1)
  arbiter.in.head.valid := io.in.valid
  arbiter.in.head.bits := io.in.bits
  io.inReady := arbiter.in.head.ready
  io.out <> arbiter.out
  io.chosen := arbiter.chosen
}

// Exercise both registration orders: SvMux1H is a direct ExtModule here and
// also an SV-only dependency inside SvRRArbiter. All visible payloads stay typed.
private[test] class SvRRArbiterMuxDependencyTop(muxFirst: Boolean) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(new Bundle {
    val arb = new ArbiterIO(Vec(3, new WrapperTestPacket(17)), 3)
    val sel = Input(UInt(3.W))
    val muxOut = Output(Vec(3, new WrapperTestPacket(17)))
  })

  private val pair = if (muxFirst) {
    val mux = Module(new SvMux1HImpl(51, 3))
    (Module(new SvRRArbiter(Vec(3, new WrapperTestPacket(17)), 3)), mux)
  } else {
    val arbiter = Module(new SvRRArbiter(Vec(3, new WrapperTestPacket(17)), 3))
    (arbiter, Module(new SvMux1HImpl(51, 3)))
  }
  io.arb <> pair._1.io
  pair._2.io.sel := io.sel
  pair._2.io.in := VecInit(io.arb.in.map(_.bits)).asUInt
  io.muxOut := pair._2.io.out.asTypeOf(io.muxOut)
}

private[test] case class SvRRArbiterDecision(
  chosen:   Int,
  ready:    Seq[Boolean],
  outValid: Boolean,
  fire:     Boolean
)

class SvRRArbiterSpec extends SimTestSpec {
  private def withPrefix[T](prefix: String)(body: => T): T = GlobalData.synchronized {
    val previous = GlobalData.prefix
    GlobalData.prefix = prefix
    try body finally GlobalData.prefix = previous
  }

  private def moduleBody(rtl: String, name: String): String =
    (raw"(?s)\bmodule\s+" + java.util.regex.Pattern.quote(name) + raw"\b.*?\bendmodule\b").r
      .findFirstIn(rtl).getOrElse(fail(s"Missing RTL module $name"))

  private def decide(c: SvRRArbiterCase, valid: Seq[Boolean], outReady: Boolean, lastGrant: Int): SvRRArbiterDecision = {
    val chosen = (1 to c.n).map(offset => (lastGrant + offset) % c.n).find(valid).getOrElse(0)
    val outValid = valid.exists(identity)
    val ready = (0 until c.n).map(i => outReady && outValid && i == chosen)
    SvRRArbiterDecision(chosen, ready, outValid, outValid && outReady)
  }

  private def pokeInputs(
    port:     SvRRArbiterTestPort,
    valid:    Seq[Boolean],
    payloads: Seq[BigInt],
    outReady: Boolean
  ): Unit = {
    port.in.zip(valid).zip(payloads).foreach { case ((input, v), bits) =>
      input.valid.poke(v.B)
      pokePayload(input.bits, bits)
    }
    port.actualOut.ready.poke(outReady.B)
    port.referenceOut.ready.poke(outReady.B)
  }

  private def checkDecision(
    port:   SvRRArbiterTestPort,
    d:      SvRRArbiterDecision,
    bits:   Seq[BigInt],
    clue:   String
  ): Unit = {
    port.actualOut.valid.expect(d.outValid.B, s"$clue actual valid")
    port.referenceOut.valid.expect(d.outValid.B, s"$clue reference valid")
    port.actualChosen.expect(d.chosen.U, s"$clue actual chosen")
    port.referenceChosen.expect((if (d.outValid) d.chosen else port.in.size - 1).U, s"$clue reference chosen")
    d.ready.zipWithIndex.foreach { case (ready, i) =>
      port.actualInReady(i).expect((if (port.in.size == 1) port.actualOut.ready.peek().litToBoolean else ready).B,
        s"$clue actual ready[$i]")
      // Native RR may also ready invalid lanes; valid-lane transfers must agree.
      if (port.in(i).valid.peek().litValue != 0)
        port.referenceInReady(i).expect(ready.B, s"$clue reference ready[$i]")
    }
    expectPayload(port.actualOut.bits, if (port.in.size == 1 || d.outValid) bits(d.chosen) else BigInt(0),
      s"$clue actual bits")
    expectPayload(port.referenceOut.bits, bits(if (d.outValid) d.chosen else bits.size - 1), s"$clue reference bits")
  }

  private def resetDut(
    dut:    SvRRArbiterTestBank,
    cases:  Seq[SvRRArbiterCase],
    models: Array[Option[Int]]
  ): Unit = {
    cases.zip(dut.io).foreach { case (c, port) =>
      pokeInputs(port, Seq.fill(c.n)(false), Seq.fill(c.n)(BigInt(0)), outReady = false)
    }
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    cases.indices.foreach(i => models(i) = Some(0))
  }

  private def exercise(cases: Seq[SvRRArbiterCase], randomCycles: Int): Unit = {
    val seed = testSeed(0x4a1b3L)
    val cycles = testCycles(randomCycles)
    val rng = new Random(seed)
    info(s"seed=$seed cycles=$cycles cases=${cases.size}")
    simulate(new SvRRArbiterTestBank(cases)) { dut =>
      val models = Array.fill[Option[Int]](cases.size)(None)
      resetDut(dut, cases, models)

      def checkCycle(
        valid:    Seq[Seq[Boolean]],
        outReady: Seq[Boolean],
        payloads: Seq[Seq[BigInt]],
        step:     Boolean,
        clue:     String
      ): Seq[SvRRArbiterDecision] = {
        val decisions = cases.indices.map { i =>
          val lastGrant = models(i).getOrElse(throw new IllegalStateException(s"uninitialized model $i"))
          val d = decide(cases(i), valid(i), outReady(i), lastGrant)
          pokeInputs(dut.io(i), valid(i), payloads(i), outReady(i))
          checkDecision(dut.io(i), d, payloads(i), s"$clue case=${cases(i)} lane=$i")
          d
        }
        if (step) {
          dut.clock.step()
          decisions.zipWithIndex.foreach { case (d, i) =>
            if (d.fire) models(i) = Some(d.chosen)
          }
        }
        decisions
      }

      def payloadSet(): Seq[Seq[BigInt]] = cases.map(c => Seq.fill(c.n)(BigInt(c.width, rng)))

      // Idle and backpressure must not move the round-robin pointer.
      for (cycle <- 0 until 3)
        checkCycle(cases.map(c => Seq.fill(c.n)(false)), cases.map(_ => true), payloadSet(), step = true,
          clue = s"idle cycle=$cycle")
      for (cycle <- 0 until 4)
        checkCycle(cases.map(c => Seq.fill(c.n)(true)), cases.map(_ => false), payloadSet(), step = true,
          clue = s"backpressure cycle=$cycle")

      // Sustained all-valid traffic exposes fairness and wrap-around.
      for (cycle <- 0 until cases.map(_.n).max + 3)
        checkCycle(cases.map(c => Seq.fill(c.n)(true)), cases.map(_ => true), payloadSet(), step = true,
          clue = s"rotation cycle=$cycle")

      // Randomized valid masks, ready backpressure and payload bits.
      for (cycle <- 0 until cycles) {
        val valid = cases.map { c => Seq.fill(c.n)(rng.nextBoolean()) }
        val ready = cases.map(_ => rng.nextBoolean())
        checkCycle(valid, ready, payloadSet(), step = true, clue = s"random cycle=$cycle")
      }

      // A reset while running always returns the last-grant lane index to zero.
      resetDut(dut, cases, models)
      checkCycle(cases.map(c => Seq.fill(c.n)(true)), cases.map(_ => true), payloadSet(), step = false,
        clue = "post-reset")
    }
  }

  describe("SvRRArbiter") {
    it("matches native async RRArbiter and the software scoreboard for Bundle and Vec[Bundle]") {
      exercise(Seq(
        SvRRArbiterCase(5, 1),
        SvRRArbiterCase(17, 3),
        SvRRArbiterCase(51, 3, payload = "vecBundle", factory = true),
        SvRRArbiterCase(65, 5),
        SvRRArbiterCase(51, 7, payload = "vecBundle")
      ), randomCycles = 256)
    }

    it("preserves one-hot pointer rotation across bits 31 and 63 under backpressure") {
      exercise(Seq(
        SvRRArbiterCase(5, 33),
        SvRRArbiterCase(17, 65, factory = true)
      ), randomCycles = 128)
    }

    it("starts at zero, ignores backpressure, and resets asynchronously without a clock edge") {
      val c = SvRRArbiterCase(17, 3)
      val cases = Seq(c)
      val bits = (0 until c.n).map(i => BigInt(i + 1))
      simulate(new SvRRArbiterTestBank(cases)) { dut =>
        val port = dut.io(0)
        val models = Array.fill[Option[Int]](cases.size)(None)
        resetDut(dut, cases, models)

        // Reset establishes last_grant=0, so all-valid traffic starts at lane 1.
        pokeInputs(port, Seq.fill(c.n)(true), bits, outReady = true)
        val first = decide(c, Seq.fill(c.n)(true), outReady = true, lastGrant = 0)
        checkDecision(port, first, bits, "first grant after reset")
        dut.clock.step()
        models(0) = Some(first.chosen)

        // Backpressure must hold last_grant at its nonzero value.
        for (cycle <- 0 until 4) {
          val valid = Seq.fill(c.n)(true)
          val d = decide(c, valid, outReady = false, lastGrant = models(0).get)
          pokeInputs(port, valid, bits, outReady = false)
          checkDecision(port, d, bits, s"backpressure cycle=$cycle")
          dut.clock.step()
        }
        assert(models(0).contains(1))

        // Assert async reset with no intervening clock edge.
        val held = decide(c, Seq.fill(c.n)(true), outReady = false, lastGrant = 1)
        pokeInputs(port, Seq.fill(c.n)(true), bits, outReady = false)
        dut.reset.poke(true.B)
        val resetDecision = decide(c, Seq.fill(c.n)(true), outReady = false, lastGrant = 0)
        checkDecision(port, resetDecision, bits, "async reset without clock edge")
        assert(held.chosen == 2)

        dut.clock.step()
        checkDecision(port, resetDecision, bits, "async reset after clock edge")
        dut.reset.poke(false.B)
        checkDecision(port, resetDecision, bits, "async reset deasserted")
      }
    }

    it("handles single-input Bundle and Vec[Bundle] through wrapper and factory") {
      def checkWrapper(gen: => Data, bits: BigInt, clue: String): Unit =
        simulate(new SvRRArbiterN1WrapperTop(gen)) { dut =>
          dut.io.in.valid.poke(true.B)
          pokePayload(dut.io.in.bits, bits)
          dut.io.out.ready.poke(true.B)
          dut.io.out.valid.expect(true.B, clue)
          dut.io.inReady.expect(true.B, clue)
          dut.io.chosen.expect(0.U, clue)
          expectPayload(dut.io.out.bits, bits, clue)

          dut.io.in.valid.poke(false.B)
          dut.io.out.ready.poke(false.B)
          dut.io.out.valid.expect(false.B, clue)
          dut.io.inReady.expect(false.B, clue)
          expectPayload(dut.io.out.bits, bits, s"$clue invalid bits")
        }

      def checkFactory(gen: => Data, bits: BigInt, clue: String): Unit =
        simulate(new SvRRArbiterN1FactoryTop(gen)) { dut =>
          dut.io.in.valid.poke(true.B)
          pokePayload(dut.io.in.bits, bits)
          dut.io.out.ready.poke(true.B)
          dut.io.out.valid.expect(true.B, clue)
          dut.io.inReady.expect(true.B, clue)
          dut.io.chosen.expect(0.U, clue)
          expectPayload(dut.io.out.bits, bits, clue)

          dut.io.in.valid.poke(false.B)
          dut.io.out.ready.poke(false.B)
          dut.io.out.valid.expect(false.B, clue)
          dut.io.inReady.expect(false.B, clue)
          expectPayload(dut.io.out.bits, bits, s"$clue invalid bits")
        }

      val cases = Seq(
        ("bundle", new WrapperTestPacket(17), BigInt("1a5c3", 16)),
        ("vecBundle", Vec(3, new WrapperTestPacket(17)), BigInt("1a5c3d7e9", 16))
      )
      cases.foreach { case (shape, gen, bits) =>
        checkWrapper(gen, bits, s"n=1 wrapper $shape")
        checkFactory(gen, bits, s"n=1 factory $shape")
      }
    }

    it("bypasses the shared core and forwards valid, bits and ready for one input") {
      val rtl = ChiselStage.emitSystemVerilog(new SvRRArbiterN1FactoryTop(new WrapperTestPacket(17)))
      assert(!rtl.contains("module SvLivelockFreeArbiter #("))
      assert(!rtl.contains("module SvRRArbiterWrapper"))
      simulate(new SvRRArbiterN1FactoryTop(new WrapperTestPacket(17))) { dut =>
        pokePayload(dut.io.in.bits, BigInt(31))
        for (valid <- Seq(false, true); ready <- Seq(false, true)) {
          dut.io.in.valid.poke(valid.B)
          dut.io.out.ready.poke(ready.B)
          dut.io.inReady.expect(ready.B)
          dut.io.out.valid.expect(valid.B)
          dut.io.chosen.expect(0.U)
          expectPayload(dut.io.out.bits, BigInt(31))
        }
      }
    }

    it("exhaustively matches valid-lane grants and native arbitration order") {
      val cases = Seq(SvRRArbiterCase(5, 1),
        SvRRArbiterCase(17, 3),
        SvRRArbiterCase(13, 5, factory = true))
      val seed = testSeed(0xa11ceL)
      val rng = new Random(seed)
      info(s"seed=$seed exhaustive valid masks")
      simulate(new SvRRArbiterTestBank(cases)) { dut =>
        val models = Array.fill[Option[Int]](cases.size)(Some(0))
        resetDut(dut, cases, models)
        cases.zip(dut.io).foreach { case (c, port) => pokeInputs(port, Seq.fill(c.n)(true), Seq.fill(c.n)(BigInt(0)), false) }
        dut.clock.step()
        cases.indices.foreach(i => models(i) = Some(0))

        cases.zipWithIndex.foreach { case (c, caseIndex) =>
          val port = dut.io(caseIndex)
          for (lastGrant <- 0 until c.n) {
            // A lone lane always wins and writes the requested pointer value.
            val setValid = (0 until c.n).map(_ == lastGrant)
            val setBits = (0 until c.n).map(i => BigInt(c.width, rng) ^ i)
            pokeInputs(port, setValid, setBits, outReady = true)
            dut.clock.step()
            models(caseIndex) = Some(lastGrant)

            for (mask <- 0 until (1 << c.n); ready <- Seq(false, true)) {
              val valid = (0 until c.n).map(i => ((mask >> i) & 1) != 0)
              val bits = (0 until c.n).map(i => BigInt(c.width, rng) ^ (BigInt(i + 1) << (i % c.width)))
              val d = decide(c, valid, ready, lastGrant)
              pokeInputs(port, valid, bits, ready)
              checkDecision(port, d, bits, s"case=$c lastGrant=$lastGrant mask=$mask ready=$ready")
            }
          }
        }
      }
    }

    it("inlines wrappers and emits one prefixed parameterized module for multiple parameters") {
      for (prefix <- Seq("soc_", "")) withPrefix(prefix) {
        val rtl = ChiselStage.emitSystemVerilog(new SvRRArbiterTestBank(Seq(
          SvRRArbiterCase(8, 2),
          SvRRArbiterCase(17, 3, factory = true)
        )))
        val sourcePattern = ("(?m)^\\s*module\\s+(" + prefix + "SvLivelockFreeArbiter(?:_[0-9]+)?)\\b").r
        val names: Seq[String] = sourcePattern.findAllMatchIn(rtl).map(m => m.group(1)).toList
        assert(names == Seq(prefix + "SvLivelockFreeArbiter"), names.mkString(", "))
        assert(rtl.contains("parameter integer DATA_WIDTH"))
        assert(rtl.contains("parameter integer INPUT_SIZE"))
        assert(!rtl.contains("io_last_grant"))
        assert(rtl.contains("always_ff @(posedge clock or posedge reset)"))
        assert(!rtl.contains("arbiter_lastGrant"), "typed wrapper must not hold lastGrant")
        assert(!rtl.contains(s"module ${prefix}SvRRArbiter("), "typed wrapper must be inlined")
      }
    }

    it("collects and prefixes the shared encoders and output mux through ctx.ref") {
      for (prefix <- Seq("", "soc_", "chip_")) withPrefix(prefix) {
        val registry = new VlogRegistry
        val sources = registry.sources(SvLivelockFreeArbiterImpl)
        val expected = Seq("SvPrefixReduceOR", "SvOHToMask", "SvPriorityEncoder1H", "SvMux1H", "SvOHToUInt", "SvLivelockFreeArbiter").map(prefix + _)
        assert(sources.map(_.moduleName) == expected)
        assert(sources.map(_.fileName) == expected.map(_ + ".sv"))
        assert(registry.sources(SvLivelockFreeArbiterImpl) == sources, "closure must be deterministic")
        val arbiter = sources.last.text
        assert(arbiter.contains(s"${prefix}SvOHToMask #(.INPUT_SIZE(INPUT_SIZE), .HIGH_MASK(1)) u_higher_mask"))
        for (instance <- Seq("u_first_valid", "u_first_priority")) {
          assert(arbiter.contains(s"${prefix}SvPriorityEncoder1H #(.INPUT_SIZE(INPUT_SIZE)) $instance"))
        }
        assert(arbiter.contains(s"${prefix}SvMux1H #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(INPUT_SIZE)) u_out_bits"))
        assert(arbiter.contains(".io_sel(selected)"))
        assert(arbiter.contains(".io_in(io_in_bits)"))
        assert(arbiter.contains(".io_out(io_out_bits)"))
        assert(arbiter.contains(s"${prefix}SvOHToUInt #(.INPUT_SIZE(INPUT_SIZE)) u_chosen"))
        assert(!arbiter.contains("in_bits[chosen]"), "data selection must use the shared mux")
      }
    }

    it("emits direct and SV-dependent mux sources once in either registration order") {
      for (muxFirst <- Seq(false, true); prefix <- Seq("", "soc_")) withPrefix(prefix) {
        val annotations = ChiselStage.elaborate(new SvRRArbiterMuxDependencyTop(muxFirst)).annotations.collect {
          case annotation: BlackBoxInlineAnno => annotation
        }.toSeq
        val expectedFiles = Seq("SvPrefixReduceOR", "SvOHToMask", "SvPriorityEncoder1H", "SvMux1H", "SvOHToUInt", "SvLivelockFreeArbiter")
          .map(name => s"$prefix$name.sv").sorted
        assert(annotations.map(_.name).sorted == expectedFiles,
          s"muxFirst=$muxFirst prefix=$prefix must register each source only once")

        val rtl = ChiselStage.emitSystemVerilog(new SvRRArbiterMuxDependencyTop(muxFirst))
        val moduleNames = raw"(?m)^\s*module\s+(\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
        assert(moduleNames.sorted == Seq("SvRRArbiterMuxDependencyTop", s"${prefix}SvLivelockFreeArbiter",
          s"${prefix}SvMux1H", s"${prefix}SvPrefixReduceOR", s"${prefix}SvPriorityEncoder1H", s"${prefix}SvOHToUInt", s"${prefix}SvOHToMask").sorted,
          "only the top and six shared SV definitions should survive")
        val top = moduleBody(rtl, "SvRRArbiterMuxDependencyTop")
        val arbiter = moduleBody(rtl, s"${prefix}SvLivelockFreeArbiter")
        val muxInstance = (raw"(?m)^\s*" + prefix + raw"SvMux1H\s*#\(").r
        assert(muxInstance.findAllIn(top).size == 1, "preserve the direct mux instance")
        assert(muxInstance.findAllIn(arbiter).size == 1, "preserve the SV-only output mux instance")
        assert(!top.contains("last_grant") && !top.contains("posedge"), "state must stay inside the SV core")
        assert(raw"(?m)^\s*reg\b".r.findFirstIn(top).isEmpty, "no Scala state may survive inlining")
        assert(arbiter.contains("always_ff @(posedge clock or posedge reset)"))
        assert(arbiter.contains("pointer <= INPUT_SIZE'(1);"))
      }
    }

    it("simulates the shared mux alongside direct instances and zeroes idle payloads") {
      val c = SvRRArbiterCase(51, 3, payload = "vecBundle")
      val seed = testSeed(0x4d581L)
      val cycles = math.max(8, testCycles(64))
      for (muxFirst <- Seq(false, true)) {
        val rng = new Random(seed)
        simulate(new SvRRArbiterMuxDependencyTop(muxFirst)) { dut =>
          val arb = dut.io.arb
          arb.in.foreach { lane =>
            lane.valid.poke(false.B)
            pokePayload(lane.bits, 0)
          }
          arb.out.ready.poke(false.B)
          dut.io.sel.poke(0.U)
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)
          var lastGrant = 0

          for (cycle <- 0 until cycles) {
            val mask = if (cycle < 8) cycle else rng.nextInt(8)
            val sel = if (cycle < 8) 7 - cycle else rng.nextInt(8)
            val valid = (0 until c.n).map(i => (mask & (1 << i)) != 0)
            val bits = Seq.fill(c.n)(BigInt(c.width, rng))
            val ready = cycle % 4 != 0
            val d = decide(c, valid, ready, lastGrant)
            val clue = s"muxFirst=$muxFirst cycle=$cycle mask=$mask sel=$sel"
            arb.in.zipWithIndex.foreach { case (lane, i) =>
              lane.valid.poke(valid(i).B)
              pokePayload(lane.bits, bits(i))
            }
            arb.out.ready.poke(ready.B)
            dut.io.sel.poke(sel.U)
            arb.chosen.expect(d.chosen.U, clue)
            arb.out.valid.expect(d.outValid.B, clue)
            arb.in.zip(d.ready).foreach { case (lane, r) => lane.ready.expect(r.B, clue) }
            // The shared core returns zero data when no lane is valid.
            expectPayload(arb.out.bits, if (d.outValid) bits(d.chosen) else BigInt(0), s"$clue arbiter")
            val muxBits = bits.zipWithIndex.collect { case (word, i) if (sel & (1 << i)) != 0 => word }
              .foldLeft(BigInt(0))(_ | _)
            expectPayload(dut.io.muxOut, muxBits, s"$clue direct mux")
            dut.clock.step()
            if (d.fire) lastGrant = d.chosen
          }
        }
      }
    }

    it("rejects nonpositive dimensions and unknown payload widths") {
      for ((width, n) <- Seq((0, 1), (-1, 2), (8, 0), (8, -1))) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val bad = Module(new SvLivelockFreeArbiterImpl(width, n))
          })
        }
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module {
          val bad = Module(new SvRRArbiter(UInt(), 2))
        })
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module {
          val bad = Module(new SvRRArbiter(new WrapperTestPacket(17), 0))
        })
      }
    }
  }
}
