package xs.utils.test.chiselsim

import chisel3._
import chisel3.util._
import xs.utils.test.WrapperTestPayload
import xs.utils.queue.{FastQueue, FastQueueNoX}

import scala.collection.mutable
import scala.util.Random

private[test] case class FastQueueCase(
  depth: Int,
  width: Int,
  pipe: Boolean = true,
  flow: Boolean = false,
  noX: Boolean = false,
  payload: String = "bundle",
  factory: Boolean = false
)

private[test] class FastQueueTestPort(c: FastQueueCase) extends Bundle {
  val enq = Flipped(Decoupled(WrapperTestPayload(c.width, c.payload)))
  val deq = Decoupled(WrapperTestPayload(c.width, c.payload))
  val count = Output(UInt(log2Ceil(c.depth + 1).W))
  val free = Output(UInt(log2Ceil(c.depth + 1).W))
  val waterline = Output(UInt((c.depth + 1).W))
}

// Keep aggregate ports through the entire harness; only the wrapper packs data.
private[test] class FastQueueTestBank(cases: Seq[FastQueueCase]) extends Module {
  val io = IO(MixedVec(cases.map(new FastQueueTestPort(_))))

  cases.zip(io).foreach { case (c, port) =>
    if (c.factory) {
      port.deq <> (if (c.noX) FastQueueNoX(port.enq, c.depth) else FastQueue(port.enq, c.depth))
      // The convenience factories return only DecoupledIO, not occupancy.
      port.count := 0.U
      port.free := 0.U
      port.waterline := 0.U
    } else {
      val q = Module(new FastQueue(chiselTypeOf(port.enq.bits), c.depth, c.pipe, c.flow, c.noX))
      q.io.enq <> port.enq
      port.deq <> q.io.deq
      port.count := q.io.count
      port.free := q.io.free
      port.waterline := q.io.waterline
    }
  }
}

/** Run: xmake test --suite=FastQueueSpec */
class FastQueueSpec extends SimTestSpec {
  private case class Input(valid: Boolean, bits: BigInt, ready: Boolean)

  private def exercise(cases: Seq[FastQueueCase], defaultSeed: Long): Unit = {
    val seed = testSeed(defaultSeed)
    val cycles = testCycles()
    info(s"seed=$seed cycles=$cycles configurations=${cases.size}")
    simulate(new FastQueueTestBank(cases)) { dut =>
      val model = cases.map(_ => mutable.Queue.empty[BigInt])
      val rng = new Random(seed)
      var cycle = 0
      val maxDepth = cases.map(_.depth).max
      val idle = cases.map(_ => Input(false, BigInt(0), false))

      def occupancy(i: Int): Unit = {
        val c = cases(i)
        val clue = s"seed=$seed cycle=$cycle case=$c"
        if (!c.factory) {
          dut.io(i).count.expect(model(i).size.U, clue)
          dut.io(i).free.expect((c.depth - model(i).size).U, clue)
          dut.io(i).waterline.expect((BigInt(1) << model(i).size).U, clue)
        }
      }

      def reset(): Unit = {
        dut.io.foreach { p =>
          p.enq.valid.poke(false.B)
          pokePayload(p.enq.bits, 0)
          p.deq.ready.poke(false.B)
        }
        dut.reset.poke(true.B)
        dut.clock.step(2)
        model.foreach(_.clear())
        dut.reset.poke(false.B)
        cases.indices.foreach { i =>
          dut.io(i).deq.valid.expect(false.B)
          dut.io(i).enq.ready.expect(true.B)
          occupancy(i)
        }
      }

      def step(inputs: Seq[Input]): Seq[Boolean] = {
        inputs.zipWithIndex.foreach { case (in, i) =>
          dut.io(i).enq.valid.poke(in.valid.B)
          pokePayload(dut.io(i).enq.bits, in.bits)
          dut.io(i).deq.ready.poke(in.ready.B)
        }
        val fired = cases.indices.map { i =>
          val c = cases(i)
          val in = inputs(i)
          val queue = model(i)
          val clue = s"seed=$seed cycle=$cycle case=$c input=$in contents=${queue.toList}"
          // PIPE is exposed by the Scala wrapper only for D1.
          val ready = queue.size < c.depth || (c.depth == 1 && c.pipe && in.ready)
          val valid = queue.nonEmpty || (c.flow && in.valid)
          dut.io(i).enq.ready.expect(ready.B, clue)
          dut.io(i).deq.valid.expect(valid.B, clue)
          occupancy(i)
          if (valid) expectPayload(dut.io(i).deq.bits, if (queue.nonEmpty) queue.front else in.bits, clue)
          else if (c.noX && !c.flow) expectPayload(dut.io(i).deq.bits, 0, clue)
          // FLOW intentionally forwards input bits whenever empty, even if invalid.
          if (queue.isEmpty && c.flow) expectPayload(dut.io(i).deq.bits, in.bits, clue)
          (in.valid && ready, in.ready && valid)
        }
        dut.clock.step()
        fired.zipWithIndex.foreach { case ((push, pop), i) =>
          // Push first also models an empty FLOW bypass (push and pop together).
          if (push) model(i).enqueue(inputs(i).bits)
          if (pop) model(i).dequeue()
        }
        cycle += 1
        cases.indices.foreach(occupancy)
        fired.map(_._1)
      }

      def data(i: Int, value: BigInt): BigInt = value & ((BigInt(1) << cases(i).width) - 1)
      def drain(): Unit = {
        for (_ <- 0 to maxDepth) step(idle.map(_.copy(ready = true)))
        assert(model.forall(_.isEmpty), s"seed=$seed: queues did not drain")
      }

      reset()
      // Empty bypass, filling, sustained full backpressure, then simultaneous I/O.
      for (n <- 0 until maxDepth + 3)
        step(cases.indices.map(i => Input(true, data(i, n + 1), true)))
      drain()
      for (n <- 0 until maxDepth)
        step(cases.indices.map(i => Input(n < cases(i).depth, data(i, n + 1), false)))
      val stalled = cases.indices.map(i => Input(true, data(i, BigInt(-1)), false))
      for (_ <- 0 until 4) step(stalled)
      var accepted = step(stalled.map(_.copy(ready = true)))
      var attempts = 0
      while (!accepted.forall(identity) && attempts < maxDepth + 2) {
        val next = step(stalled.zip(accepted).map { case (in, done) => in.copy(valid = !done, ready = true) })
        accepted = accepted.zip(next).map { case (a, b) => a || b }
        attempts += 1
      }
      assert(accepted.forall(identity), s"seed=$seed: stalled producer did not transfer")
      drain()
      // Repeated wraparound; hold producer valid/data until its transfer occurs.
      var pending = idle
      for (_ <- 0 until cycles) {
        val inputs = pending.zipWithIndex.map { case (in, i) =>
          if (in.valid) in.copy(ready = rng.nextBoolean())
          else Input(rng.nextBoolean(), BigInt(cases(i).width, rng), rng.nextBoolean())
        }
        val pushed = step(inputs)
        pending = inputs.zip(pushed).map { case (in, push) => in.copy(valid = in.valid && !push) }
      }
      attempts = 0
      while (pending.exists(_.valid) && attempts < maxDepth + 2) {
        val pushed = step(pending.map(_.copy(ready = true)))
        pending = pending.zip(pushed).map { case (in, push) => in.copy(valid = in.valid && !push) }
        attempts += 1
      }
      assert(!pending.exists(_.valid), s"seed=$seed: pending producer did not transfer")
      drain()
      // Reset while full must discard every old entry, then allow reuse.
      for (n <- 0 until maxDepth)
        step(cases.indices.map(i => Input(n < cases(i).depth, data(i, n + 1), false)))
      reset()
      step(cases.indices.map(i => Input(true, data(i, BigInt(-1)), false)))
      drain()
    }
  }

  describe("FastQueue with ChiselSim and Verilator") {
    for (depth <- Seq(1, 2, 3, 4, 5, 6, 8, 33)) {
      it(s"preserves FIFO data and occupancy at depth $depth across parameter combinations") {
        val cases = for {
          width <- Seq(1, 8, 32, 65)
          flow <- Seq(false, true)
          noX <- Seq(false, true)
          pipe <- (if (depth == 1) Seq(false, true) else Seq(true))
        } yield FastQueueCase(depth, width, pipe, flow, noX)
        exercise(cases, 0x5eedL + depth)
      }
    }

    it("supports mixed DN depths with one parameterized RTL module") {
      val cases = for {
        depth <- Seq(4, 5, 6, 8, 33)
        width <- Seq(1, 8, 32, 65)
        flow <- Seq(false, true)
        noX <- Seq(false, true)
      } yield FastQueueCase(depth, width, flow = flow, noX = noX)
      exercise(cases, 0xD00DL)
    }

    it("round-trips Bundle and Vec[Bundle] fields through typed and factory interfaces") {
      val cases = for {
        (payload, width) <- Seq("bundle" -> 1, "bundle" -> 9, "bundle" -> 17,
          "bundle" -> 65, "vecBundle" -> 15, "vecBundle" -> 51, "vecBundle" -> 66)
        depth <- Seq(1, 2, 3, 4)
        factory <- Seq(false, true)
        noX <- Seq(false, true)
      } yield FastQueueCase(depth, width, noX = noX, payload = payload, factory = factory)
      exercise(cases, 0x12345678L)
    }
  }
}
