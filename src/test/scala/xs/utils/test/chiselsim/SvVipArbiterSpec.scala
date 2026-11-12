package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{ArbiterIO, Decoupled, MixedVec, Valid, log2Ceil}
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import xs.utils.GlobalData
import xs.utils.arb.{ConditionVipArbiter, VipArbiter}
import xs.utils.test.WrapperTestPayload
import xs.utils.verilog._

import scala.util.Random

private[test] case class VipCase(
  size: Int, width: Int = 17, shape: String = "bundle", api: String = "sv",
  roundRobin: Boolean = false
)

private[test] class VipPort(c: VipCase) extends Bundle {
  val in = Input(Vec(c.size, Valid(WrapperTestPayload(c.width, c.shape))))
  val out = Decoupled(WrapperTestPayload(c.width, c.shape))
  val ready = Output(UInt(c.size.W))
  val chosen = Output(UInt(math.max(1, log2Ceil(c.size)).W))
}

private[test] class VipBank(cases: Seq[VipCase]) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(MixedVec(cases.map(new VipPort(_))))
  cases.zip(io).foreach { case (c, port) =>
    val gen = chiselTypeOf(port.in.head.bits)
    val arb: ArbiterIO[Data] = if (c.roundRobin) {
      c.api match {
        case "factory" => SvRRArbiter(gen, c.size)
        case "shared" => SvLivelockFreeArbiter(gen, c.size, roundRobin = true)
        case "rr" => Module(new xs.utils.arb.RRArbiter(gen, c.size)).io
        case _ => Module(new SvRRArbiter(gen, c.size)).io
      }
    } else c.api match {
      case "core" =>
        val arb = Wire(new ArbiterIO(gen, c.size))
        val core = Module(new SvLivelockFreeArbiterImpl(c.width, c.size, c.roundRobin))
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
      case "vip" =>
        Module(new VipArbiter(gen, c.size)).io
      case "factory" => SvVipArbiter(gen, c.size)
      case "shared" => SvLivelockFreeArbiter(gen, c.size)
      case _ => Module(new SvVipArbiter(gen, c.size)).io
    }
    arb.in.zip(port.in).foreach { case (sink, in) => sink.valid := in.valid; sink.bits := in.bits }
    port.out <> arb.out
    port.ready := VecInit(arb.in.map(_.ready)).asUInt
    port.chosen := arb.chosen
  }
}

private[test] class VipFactories extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(new Bundle {
    val in = Input(Vec(3, Valid(WrapperTestPayload(51, "vecBundle"))))
    val out = Vec(2, Decoupled(WrapperTestPayload(51, "vecBundle")))
    val ready = Output(Vec(2, UInt(3.W)))
  })
  for (api <- 0 until 2) {
    val inputs = Wire(Vec(3, Decoupled(chiselTypeOf(io.in.head.bits))))
    inputs.zip(io.in).foreach { case (sink, in) => sink.valid := in.valid; sink.bits := in.bits }
    val out = api match {
      case 0 => SvVipArbiter(inputs.toSeq)
      case _ => VipArbiter(inputs.toSeq)
    }
    io.out(api) <> out
    io.ready(api) := VecInit(inputs.map(_.ready)).asUInt
  }
}

class SvVipArbiterSpec extends SimTestSpec {
  // Independent index model: walk clockwise from VIP, without OH masks/encoders.
  private def choice(valid: BigInt, vip: Int, n: Int): Int =
    if (valid.testBit(vip)) vip else (0 until n).find(valid.testBit).getOrElse(0)

  private def nextVip(valid: BigInt, vip: Int, n: Int, ready: Boolean): Int = {
    val next = (1 until n).map(offset => (vip + offset) % n).find(valid.testBit)
    if (!valid.testBit(vip) || ready) next.getOrElse(vip) else vip
  }

  private def drive(port: VipPort, valid: BigInt, ready: Boolean, bits: Seq[BigInt]): Unit = {
    port.in.zipWithIndex.foreach { case (lane, i) =>
      lane.valid.poke(valid.testBit(i).B)
      pokePayload(lane.bits, bits(i))
    }
    port.out.ready.poke(ready.B)
  }

  private def check(port: VipPort, c: VipCase, valid: BigInt, vip: Int, ready: Boolean,
    bits: Seq[BigInt], clue: String): Int = {
    val selected = choice(valid, vip, c.size)
    port.out.valid.expect((valid != 0).B, clue)
    port.chosen.expect(selected.U, clue)
    port.ready.expect((if (ready && (c.size == 1 || valid != 0)) BigInt(1) << selected else BigInt(0)).U, clue)
    expectPayload(port.out.bits, if (c.size == 1 || valid != 0) bits(selected) else BigInt(0), clue)
    selected
  }

  describe("SvVipArbiter") {
    it("exhaustively checks every VIP, request mask, ready value and next VIP for small sizes") {
      val cases = Seq(1, 2, 3, 5).map(VipCase(_))
      simulate(new VipBank(cases)) { dut =>
        cases.zip(dut.io).foreach { case (c, port) => drive(port, 0, false, Seq.fill(c.size)(BigInt(0))) }
        for ((c, index) <- cases.zipWithIndex; vip <- 0 until c.size;
          mask <- 0 until (1 << c.size); ready <- Seq(false, true)) {
          val port = dut.io(index)
          val bits = Seq.tabulate(c.size)(i => BigInt(0x1111 * (i + 1)))
          dut.reset.poke(true.B)
          dut.clock.step()
          dut.reset.poke(false.B)
          // A single requester moves an inactive VIP even while the sink stalls.
          drive(port, BigInt(1) << vip, false, bits)
          dut.clock.step()
          drive(port, mask, ready, bits)
          val clue = s"n=${c.size} vip=$vip mask=$mask ready=$ready"
          check(port, c, mask, vip, ready, bits, clue)
          val next = nextVip(mask, vip, c.size, ready)
          dut.clock.step()
          // All lanes valid exposes the registered VIP through chosen.
          drive(port, (BigInt(1) << c.size) - 1, false, bits)
          check(port, c, (BigInt(1) << c.size) - 1, next, false, bits, s"$clue next")
        }
      }
    }

    it("matches the model for aliases, factory, aggregate payloads and requests held until transfer") {
      val cases = Seq(VipCase(1, 1), VipCase(2, api = "vip"), VipCase(3, 51, "vecBundle", "step"),
        VipCase(5, 66, "vecBundle", "factory"), VipCase(33))
      val seed = testSeed(0x71fL)
      val cycles = testCycles(512)
      val random = new Random(seed)
      info(s"seed=$seed cycles=$cycles")
      simulate(new VipBank(cases)) { dut =>
        val vip = Array.fill(cases.size)(0)
        val valid = Array.fill(cases.size)(BigInt(0))
        val bits = cases.map(c => Array.fill(c.size)(BigInt(c.width, random)))
        val age = cases.map(c => Array.fill(c.size)(0))
        for (cycle <- 0 until cycles) {
          val grants = cases.zipWithIndex.map { case (c, index) =>
            for (i <- 0 until c.size if !valid(index).testBit(i)) {
              if (random.nextBoolean()) valid(index) = valid(index).setBit(i)
              bits(index)(i) = BigInt(c.width, random)
            }
            // Keep the highest lane pending, competing with low-index traffic.
            valid(index) = valid(index).setBit(c.size - 1)
            val ready = cycle % 13 >= 7 && random.nextBoolean()
            drive(dut.io(index), valid(index), ready, bits(index).toSeq)
            val selected = check(dut.io(index), c, valid(index), vip(index), ready, bits(index).toSeq,
              s"seed=$seed cycle=$cycle case=$c")
            for (i <- 0 until c.size) {
              if (!valid(index).testBit(i) || (ready && selected == i)) age(index)(i) = 0
              else if (ready) age(index)(i) += 1
              assert(age(index)(i) < c.size, s"starvation: cycle=$cycle case=$c lane=$i")
            }
            vip(index) = nextVip(valid(index), vip(index), c.size, ready)
            if (ready) Some(selected) else None
          }
          dut.clock.step()
          grants.zipWithIndex.foreach { case (selected, index) =>
            selected.foreach(i => valid(index) = valid(index).clearBit(i))
          }
        }
      }
    }

    it("serves every continuously pending lane within size transfers despite long backpressure") {
      val cases = for (rr <- Seq(false, true); n <- Seq(1, 2, 3, 5, 8, 33))
        yield VipCase(n, roundRobin = rr)
      simulate(new VipBank(cases)) { dut =>
        val wait = cases.map(c => Array.fill(c.size)(0))
        val served = cases.map(c => Array.fill(c.size)(0))
        for (cycle <- 0 until 33 * 4 * 11) {
          val ready = cycle % 11 == 10
          cases.zipWithIndex.foreach { case (c, index) =>
            val port = dut.io(index)
            val bits = Seq.tabulate(c.size)(i => BigInt(i + 1))
            drive(port, (BigInt(1) << c.size) - 1, ready, bits)
            port.out.valid.expect(true.B)
            val chosen = port.chosen.peek().litValue.toInt
            assert(chosen >= 0 && chosen < c.size)
            expectPayload(port.out.bits, bits(chosen))
            port.ready.expect((if (ready) BigInt(1) << chosen else BigInt(0)).U)
            if (ready) for (i <- 0 until c.size) {
              if (i == chosen) { wait(index)(i) = 0; served(index)(i) += 1 }
              else wait(index)(i) += 1
              assert(wait(index)(i) < c.size, s"liveness: n=${c.size} lane=$i cycle=$cycle")
            }
          }
          dut.clock.step()
        }
        served.foreach(counts => assert(counts.forall(_ >= 4), counts.mkString(",")))
      }
    }

    it("skips inactive VIPs under backpressure, preserves valid VIPs, wraps and resets asynchronously") {
      val c = VipCase(5)
      simulate(new VipBank(Seq(c))) { dut =>
        val port = dut.io.head
        val bits = Seq.tabulate(c.size)(i => BigInt(i + 1))
        // Establish VIP 3, then leave only lanes 1 and 4 active.
        drive(port, 8, false, bits)
        dut.clock.step()
        drive(port, 18, false, bits)
        check(port, c, 18, 3, false, bits, "fallback selects lane 1")
        dut.clock.step()
        for (_ <- 0 until 20) {
          check(port, c, 18, 4, false, bits, "VIP skips to lane 4 and holds while stalled")
          dut.clock.step()
        }
        drive(port, 18, true, bits)
        check(port, c, 18, 4, true, bits, "VIP transfer")
        dut.clock.step()
        check(port, c, 18, 1, true, bits, "wrap to lane 1")
        drive(port, 31, false, bits)
        dut.reset.poke(true.B)
        // No clock edge: the SV core uses the same reset convention as SvRRArbiter.
        check(port, c, 31, 0, false, bits, "asynchronous reset")
        dut.reset.poke(false.B)
        check(port, c, 31, 0, false, bits, "restart at lane 0")
      }
    }

    it("keeps all sequence factories equivalent with typed payloads and backpressure") {
      simulate(new VipFactories) { dut =>
        var vip = 0
        for (cycle <- 0 until 48) {
          val bits = Seq.tabulate(3)(i => BigInt(i + 1) << 35)
          val ready = cycle % 4 == 3
          dut.io.in.zipWithIndex.foreach { case (lane, i) =>
            lane.valid.poke(true.B)
            pokePayload(lane.bits, bits(i))
          }
          for (api <- 0 until 2) {
            dut.io.out(api).ready.poke(ready.B)
            dut.io.out(api).valid.expect(true.B)
            expectPayload(dut.io.out(api).bits, bits(vip))
            dut.io.ready(api).expect((if (ready) 1 << vip else 0).U)
          }
          dut.clock.step()
          if (ready) vip = (vip + 1) % 3
        }
      }
    }

    it("registers shared dependencies once and prefixes cores") {
      assert(VipArbiter eq SvVipArbiter)
      assert(xs.utils.arb.RRArbiter eq SvRRArbiter)
      GlobalData.synchronized {
        val saved = GlobalData.prefix
        try for (prefix <- Seq("", "soc_")) {
          GlobalData.prefix = prefix
          val cases = Seq(VipCase(2), VipCase(3, api = "vip"), VipCase(5, api = "vip"),
            VipCase(3, api = "rr", roundRobin = true), VipCase(5, roundRobin = true),
            VipCase(3, api = "shared"))
          val expected = Seq("SvPrefixReduceOR", "SvOHToMask", "SvPriorityEncoder1H", "SvMux1H", "SvOHToUInt", "SvLivelockFreeArbiter").map(prefix + _)
          assert(new VlogRegistry().sources(SvLivelockFreeArbiterImpl).map(_.moduleName) == expected)
          val annotations = ChiselStage.elaborate(new VipBank(cases)).annotations.collect {
            case a: BlackBoxInlineAnno => a.name
          }.toSeq
          assert(annotations.sorted == expected.map(_ + ".sv").sorted)
          val rtl = ChiselStage.emitSystemVerilog(new VipBank(cases))
          val modules = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
          assert(modules.sorted == (expected :+ "VipBank").sorted)
          for (value <- Seq(0, 1)) assert(rtl.contains(s".ROUND_ROBIN($value)"))
          assert(!rtl.contains("FAST"))
          assert(!modules.exists(_.endsWith("SvFastArbiter")))
        } finally GlobalData.prefix = saved
      }
      ChiselStage.emitCHIRRTL(new Module {
        override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
        val vip: VipArbiter[UInt] = Module(new VipArbiter(UInt(8.W), 3))
        val conditional = Module(new ConditionVipArbiter(UInt(8.W), 3, (a: UInt, b: UInt) => a < b))
      })
    }

    it("rejects empty, non-hardware and invalid-width inputs before creating an SV boundary") {
      for ((width, n) <- Seq((0, 2), (-1, 2), (8, 0), (8, -1))) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new Module { val bad = Module(new SvLivelockFreeArbiterImpl(width, n)) })
        }
      }
      for (gen <- Seq(UInt(), UInt(0.W))) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new Module { val bad = Module(new SvVipArbiter(gen, 2)) })
        }
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module { val bad = SvVipArbiter(Seq.empty[chisel3.util.DecoupledIO[UInt]]) })
      }
      intercept[Exception] {
        ChiselStage.emitCHIRRTL(new Module { val bad = SvVipArbiter(Seq(Decoupled(UInt(8.W)))) })
      }
    }
  }
}
