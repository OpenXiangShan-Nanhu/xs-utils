package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{Decoupled, Valid}
import xs.utils.arb.{BaseVipArbiter, StepVipArbiter, VipArbiter}
import xs.utils.test.WrapperTestPayload
import xs.utils.verilog._

import scala.util.Random

private[test] class LatchedArbiterFactories(size: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(size, Valid(WrapperTestPayload(66, "vecBundle"))))
    val out = Vec(8, Decoupled(WrapperTestPayload(66, "vecBundle")))
    val ready = Output(Vec(8, UInt(size.W)))
  })
  for (api <- 0 until 8) {
    val inputs = Wire(Vec(size, Decoupled(chiselTypeOf(io.in.head.bits))))
    inputs.zip(io.in).foreach { case (sink, in) => sink.valid := in.valid; sink.bits := in.bits }
    val out = api match {
      case 0 => SvVipArbiter(inputs.toSeq, selLatch = true)
      case 1 => VipArbiter(inputs.toSeq, selLatch = true)
      case 2 => StepVipArbiter(inputs.toSeq, selLatch = true)
      case 3 => BaseVipArbiter(inputs.toSeq, selLatch = true)
      case 4 => SvRRArbiter(inputs.toSeq, selLatch = true)
      case 5 => xs.utils.arb.RRArbiter(inputs.toSeq, selLatch = true)
      case 6 => SvLivelockFreeArbiter(inputs.toSeq, roundRobin = false, selLatch = true)
      case 7 => SvLivelockFreeArbiter(inputs.toSeq, roundRobin = true, selLatch = true)
    }
    io.out(api) <> out
    io.ready(api) := VecInit(inputs.map(_.ready)).asUInt
  }
}

class SvLivelockFreeArbiterSpec extends SimTestSpec {
  private def drive(port: VipPort, valid: BigInt, ready: Boolean, bits: Seq[BigInt]): Unit = {
    port.in.zipWithIndex.foreach { case (lane, i) =>
      lane.valid.poke(valid.testBit(i).B)
      pokePayload(lane.bits, bits(i))
    }
    port.out.ready.poke(ready.B)
  }

  private def check(port: VipPort, selected: Option[Int], ready: Boolean, bits: Seq[BigInt], clue: String): Unit = {
    port.out.valid.expect(selected.nonEmpty.B, clue)
    port.chosen.expect(selected.getOrElse(0).U, clue)
    port.ready.expect(selected.filter(_ => ready).map(i => BigInt(1) << i).getOrElse(BigInt(0)).U, clue)
    expectPayload(port.out.bits, selected.map(bits).getOrElse(BigInt(0)), clue)
  }

  describe("SvLivelockFreeArbiter SEL_LATCH") {
    it("adds one selection cycle, holds stalled grants, recovers withdrawn requests and clears on async reset") {
      val cases = Seq(false, true).map(rr => VipCase(5, 66, "vecBundle", "shared", rr, selLatch = true))
      simulate(new VipBank(cases)) { dut =>
        val bits = Seq.tabulate(5)(i => (BigInt(i + 1) << 62) | (i + 1))
        dut.io.foreach { port =>
          drive(port, 4, false, bits)
          check(port, None, false, bits, "no combinational grant before the first edge")
        }
        dut.clock.step()
        dut.io.foreach(port => check(port, Some(2), false, bits, "registered request"))
        for (_ <- 0 until 8) {
          dut.io.foreach { port =>
            drive(port, 5, false, bits)
            check(port, Some(2), false, bits, "new contender cannot displace stalled grant")
          }
          dut.clock.step()
        }
        dut.io.foreach { port =>
          drive(port, 5, true, bits)
          check(port, Some(2), true, bits, "consume held grant")
        }
        dut.clock.step()
        dut.io.foreach { port =>
          check(port, Some(0), true, bits, "next contender selected without a bubble")
          drive(port, 16, false, bits)
          check(port, None, false, bits, "withdrawn grant cannot transfer or bypass registration")
        }
        dut.clock.step()
        dut.io.foreach(port => check(port, Some(4), false, bits, "recover a new request"))
        dut.reset.poke(true.B)
        dut.io.foreach(port => check(port, None, false, bits, "asynchronous reset without an edge"))
        dut.reset.poke(false.B)
        dut.io.foreach(port => check(port, None, false, bits, "reset leaves selection empty"))
        dut.clock.step()
        dut.io.foreach { port =>
          drive(port, 16, true, bits)
          check(port, Some(4), true, bits, "refill after reset")
        }
        dut.clock.step()
        dut.io.foreach { port =>
          drive(port, 0, true, bits)
          check(port, None, true, bits, "consumed request is not replayed")
        }
        dut.clock.step(3)
        dut.io.foreach(port => check(port, None, true, bits, "idle"))
      }
    }

    it("sustains one flit per cycle for competing lanes and a lone requester including INPUT_SIZE=1") {
      val cases = for (rr <- Seq(false, true); n <- Seq(1, 2, 3, 5, 33, 65))
        yield VipCase(n, api = if (n % 2 == 0) "factory" else "sv", roundRobin = rr, selLatch = true)
      simulate(new VipBank(cases)) { dut =>
        val bits = cases.map(c => Seq.tabulate(c.size)(i => BigInt(i + 1)))
        cases.zipWithIndex.foreach { case (c, index) =>
          drive(dut.io(index), (BigInt(1) << c.size) - 1, true, bits(index))
          check(dut.io(index), None, true, bits(index), s"initial latency $c")
        }
        dut.clock.step()
        for (cycle <- 0 until 65 * 3) {
          cases.zipWithIndex.foreach { case (c, index) =>
            val first = if (c.roundRobin) 1 else 0
            check(dut.io(index), Some((first + cycle) % c.size), true, bits(index), s"continuous cycle=$cycle $c")
          }
          dut.clock.step()
        }
        // A single continuously valid producer must also run at full rate.
        dut.reset.poke(true.B)
        cases.zipWithIndex.foreach { case (c, index) =>
          drive(dut.io(index), BigInt(1) << (c.size - 1), true, bits(index))
        }
        dut.reset.poke(false.B)
        dut.clock.step()
        for (cycle <- 0 until 24) {
          cases.zipWithIndex.foreach { case (c, index) =>
            // A regranted lane must forward its new head, not replay its old flit.
            val nextBits = bits(index).updated(c.size - 1, BigInt((cycle + 1) * 128 + c.size))
            drive(dut.io(index), BigInt(1) << (c.size - 1), true, nextBits)
            check(dut.io(index), Some(c.size - 1), true, nextBits, s"single requester cycle=$cycle $c")
          }
          dut.clock.step()
        }
      }
    }

    it("drains every small request mask without duplicates or bubbles when each fired lane clears valid") {
      val cases = for (rr <- Seq(false, true); n <- Seq(1, 2, 3, 5))
        yield VipCase(n, roundRobin = rr, selLatch = true)
      simulate(new VipBank(cases)) { dut =>
        cases.zip(dut.io).foreach { case (c, port) => drive(port, 0, false, Seq.fill(c.size)(BigInt(0))) }
        for ((c, index) <- cases.zipWithIndex; initial <- 1 until (1 << c.size)) {
          val port = dut.io(index)
          val bits = Seq.tabulate(c.size)(i => BigInt(initial * 128 + i + 1))
          var remaining = BigInt(initial)
          var expected = if (c.roundRobin)
            (1 to c.size).map(_ % c.size).find(remaining.testBit).get
          else (0 until c.size).find(remaining.testBit).get
          val seen = scala.collection.mutable.Set.empty[Int]
          dut.reset.poke(true.B)
          dut.clock.step()
          dut.reset.poke(false.B)
          drive(port, remaining, true, bits)
          check(port, None, true, bits, s"initial selection latency case=$c mask=$initial")
          dut.clock.step()

          // Model table entries: each successful transfer invalidates that entry
          // immediately after the edge. Every remaining entry must get one turn.
          for (beat <- 0 until remaining.bitCount) {
            val clue = s"case=$c mask=$initial beat=$beat remaining=$remaining"
            check(port, Some(expected), true, bits, clue)
            assert(seen.add(expected), s"duplicate entry $clue")
            dut.clock.step()
            remaining = remaining.clearBit(expected)
            drive(port, remaining, true, bits)
            if (remaining != 0)
              expected = (1 to c.size).map(offset => (expected + offset) % c.size).find(remaining.testBit).get
          }
          assert(remaining == 0 && seen.size == BigInt(initial).bitCount)
          for (_ <- 0 until 2) {
            check(port, None, true, bits, s"no replay after draining case=$c mask=$initial")
            dut.clock.step()
          }
        }
      }
    }

    it("preserves packets and bounded fairness with persistent requests, random traffic and long backpressure") {
      val cases = for (rr <- Seq(false, true); n <- Seq(1, 3, 5, 33))
        yield VipCase(n, 66, "vecBundle", "factory", rr, selLatch = true)
      val seed = testSeed(0x1a7c4L)
      val cycles = testCycles(600)
      val random = new Random(seed)
      info(s"seed=$seed cycles=$cycles")
      simulate(new VipBank(cases)) { dut =>
        val pending = cases.map(c => Array.fill[Option[BigInt]](c.size)(None))
        val age = cases.map(c => Array.fill(c.size)(0))
        val stalled = Array.fill[Option[Int]](cases.size)(None)
        val delivered = cases.map(_ => scala.collection.mutable.Set.empty[BigInt])
        val sent = Array.fill(cases.size)(0)
        var serial = BigInt(1)
        val drainCycles = cases.map(_.size).max * 2 + 4
        for (cycle <- 0 until cycles + drainCycles) {
          val grants = cases.zipWithIndex.map { case (c, index) =>
            val draining = cycle >= cycles
            for (i <- 0 until c.size if pending(index)(i).isEmpty && !draining) {
              if (i == c.size - 1 || random.nextBoolean()) {
                pending(index)(i) = Some((serial << 32) | BigInt(32, random))
                serial += 1
                sent(index) += 1
              }
            }
            val mask = pending(index).zipWithIndex.foldLeft(BigInt(0)) {
              case (m, (entry, i)) => if (entry.nonEmpty) m.setBit(i) else m
            }
            val bits = pending(index).map(_.getOrElse(BigInt(0))).toSeq
            val ready = draining || (cycle % 23 >= 15 && random.nextBoolean())
            val port = dut.io(index)
            drive(port, mask, ready, bits)
            val selected = if (port.out.valid.peek().litValue == 0) None else Some(port.chosen.peek().litValue.toInt)
            val clue = s"seed=$seed cycle=$cycle case=$c"
            selected.foreach(i => assert(i < c.size && pending(index)(i).nonEmpty, s"phantom grant $clue lane=$i"))
            stalled(index).foreach(i => assert(selected.contains(i), s"stalled selection changed $clue"))
            check(port, selected, ready, bits, clue)
            val fired = selected.filter(_ => ready)
            fired.foreach { i =>
              assert(delivered(index).add(bits(i)), s"duplicate packet $clue lane=$i")
              for (j <- 0 until c.size) {
                if (j == i || pending(index)(j).isEmpty) age(index)(j) = 0
                else age(index)(j) += 1
                assert(age(index)(j) < c.size, s"starvation $clue lane=$j")
              }
            }
            stalled(index) = if (ready) None else selected
            fired
          }
          dut.clock.step()
          grants.zipWithIndex.foreach { case (grant, index) => grant.foreach(i => pending(index)(i) = None) }
        }
        for (index <- cases.indices) {
          assert(pending(index).forall(_.isEmpty), s"drain stalled: ${cases(index)}")
          assert(delivered(index).size == sent(index), s"lost packets: ${cases(index)}")
          assert(delivered(index).nonEmpty)
        }
      }
    }

    it("forwards selLatch through all sequence factories and compatibility aliases") {
      for (n <- Seq(1, 3)) simulate(new LatchedArbiterFactories(n)) { dut =>
        val bits = Seq.tabulate(n)(i => (BigInt(i + 1) << 61) | i)
        dut.io.in.zipWithIndex.foreach { case (lane, i) => lane.valid.poke(true.B); pokePayload(lane.bits, bits(i)) }
        dut.io.out.foreach { out => out.ready.poke(true.B); out.valid.expect(false.B) }
        dut.clock.step()
        var transfers = 0
        for (cycle <- 0 until 48) {
          val ready = cycle % 7 >= 3
          for (api <- 0 until 8) {
            val first = if (Set(4, 5, 7).contains(api)) 1 else 0
            val selected = (first + transfers) % n
            dut.io.out(api).ready.poke(ready.B)
            dut.io.out(api).valid.expect(true.B)
            expectPayload(dut.io.out(api).bits, bits(selected), s"factory=$api n=$n cycle=$cycle")
            dut.io.ready(api).expect((if (ready) BigInt(1) << selected else BigInt(0)).U)
          }
          dut.clock.step()
          if (ready) transfers += 1
        }
      }
    }
  }
}
