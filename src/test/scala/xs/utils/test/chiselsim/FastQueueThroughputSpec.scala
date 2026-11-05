package xs.utils.test.chiselsim

import chisel3._

import scala.collection.mutable

/** Occupancy means total io.count, including the dequeue register, not just DN memory.
  * Run: xmake test -s FastQueueThroughputSpec --cycles=1000
  */
class FastQueueThroughputSpec extends SimTestSpec {
  describe("FastQueue steady-state throughput") {
    for (settleCycles <- Seq(0, 4)) {
      it(s"sustains one flit per cycle with FIFO ordering after $settleCycles idle cycles") {
        val cycles = testCycles()
        val scenarios = for {
          depth <- Seq(2, 3, 4, 5, 6, 8, 33)
          initialCount <- 0 until math.min(depth, 4)
          flow <- Seq(false, true)
          noX <- Seq(false, true)
        } yield (FastQueueCase(depth, 32, flow = flow, noX = noX), initialCount)
        val cases = scenarios.map(_._1)
        val initialCounts = scenarios.map(_._2)
        val steadyCounts = scenarios.map { case (c, count) =>
          if (count == 0 && !c.flow) 1 else count
        }
        info(s"measured cycles=$cycles configurations=${cases.size} settleCycles=$settleCycles")

        simulate(new FastQueueTestBank(cases)) { dut =>
          val model = cases.map(_ => mutable.Queue.empty[BigInt])
          val pushes = Array.fill(cases.size)(0L)
          val pops = Array.fill(cases.size)(0L)
          val steadyPushes = Array.fill(cases.size)(0L)
          val steadyPops = Array.fill(cases.size)(0L)
          val all = cases.map(_ => true)
          val none = cases.map(_ => false)
          var cycle = 0L

          def clue(i: Int, phase: String): String =
            s"phase=$phase cycle=$cycle case=${cases(i)} initialCount=${initialCounts(i)} settleCycles=$settleCycles"

          def checkOccupancy(i: Int, phase: String): Unit = {
            dut.io(i).count.expect(model(i).size.U, clue(i, phase))
            dut.io(i).free.expect((cases(i).depth - model(i).size).U, clue(i, phase))
            dut.io(i).waterline.expect((BigInt(1) << model(i).size).U, clue(i, phase))
          }

          def step(enqValid: Seq[Boolean], deqReady: Boolean, phase: String, measure: Boolean = false): Unit = {
            // Monotonic per-lane IDs, with different prefixes to detect cross-lane wiring.
            val data = cases.indices.map(i => (BigInt(i + 1) << 24) ^ BigInt(pushes(i)))
            cases.indices.foreach { i =>
              dut.io(i).enq.valid.poke(enqValid(i).B)
              pokePayload(dut.io(i).enq.bits, data(i))
              dut.io(i).deq.ready.poke(deqReady.B)
            }
            val fires = cases.indices.map { i =>
              val ready = dut.io(i).enq.ready.peek().litValue == 1
              val valid = dut.io(i).deq.valid.peek().litValue == 1
              val expectedValid = model(i).nonEmpty || (cases(i).flow && enqValid(i))
              assert(ready == (model(i).size < cases(i).depth), clue(i, phase) + " enqueue ready")
              assert(valid == expectedValid, clue(i, phase) + " dequeue valid")
              checkOccupancy(i, phase)
              if (valid) {
                val expected = model(i).headOption.getOrElse(data(i))
                expectPayload(dut.io(i).deq.bits, expected, clue(i, phase) + " FIFO order")
              }
              val push = enqValid(i) && ready
              val pop = deqReady && valid
              if (measure) {
                assert(push && pop, clue(i, phase) + " throughput bubble")
                dut.io(i).count.expect(steadyCounts(i).U, clue(i, phase) + " steady occupancy")
              }
              (push, pop)
            }
            dut.clock.step()
            fires.zipWithIndex.foreach { case ((push, pop), i) =>
              // Push-before-pop also models the empty FLOW bypass correctly.
              if (push) { model(i).enqueue(data(i)); pushes(i) += 1 }
              if (pop) { model(i).dequeue(); pops(i) += 1 }
              if (measure && push) steadyPushes(i) += 1
              if (measure && pop) steadyPops(i) += 1
              checkOccupancy(i, phase)
            }
            cycle += 1
          }

          dut.io.foreach { p =>
            p.enq.valid.poke(false.B)
            pokePayload(p.enq.bits, 0)
            p.deq.ready.poke(false.B)
          }
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)
          cases.indices.foreach(i => checkOccupancy(i, "reset"))

          // Align the last prefill transfer across all nonempty lanes, so the
          // zero-idle case tests freshly filled queues before internal settling.
          val maxInitialCount = initialCounts.max
          for (n <- 0 until maxInitialCount)
            step(initialCounts.map(count => n >= maxInitialCount - count), false, "prefill")
          for (_ <- 0 until settleCycles) step(none, false, "settle")
          cases.indices.foreach { i =>
            dut.io(i).count.expect(initialCounts(i).U, clue(i, "initial occupancy"))
          }

          // Exactly one startup cycle, not an unbounded wait for valid:
          // empty !FLOW must accept but not emit; all other lanes must do both.
          step(all, true, "startup")
          cases.indices.foreach { i =>
            assert(pushes(i) == initialCounts(i) + 1L, clue(i, "startup push"))
            val startupPops = if (initialCounts(i) == 0 && !cases(i).flow) 0L else 1L
            assert(pops(i) == startupPops, clue(i, "startup pop"))
            dut.io(i).count.expect(steadyCounts(i).U, clue(i, "startup occupancy"))
          }

          for (_ <- 0 until cycles) step(all, true, "steady", measure = true)
          cases.indices.foreach { i =>
            assert(steadyPushes(i) == cycles.toLong, clue(i, "measured pushes"))
            assert(steadyPops(i) == cycles.toLong, clue(i, "measured pops"))
          }

          for (_ <- 0 to steadyCounts.max) step(none, true, "drain")
          cases.indices.foreach { i =>
            assert(model(i).isEmpty && pushes(i) == pops(i), clue(i, "lossless drain"))
          }
        }
      }
    }
  }
}
