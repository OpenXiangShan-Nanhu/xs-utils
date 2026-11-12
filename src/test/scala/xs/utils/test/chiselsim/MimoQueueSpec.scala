package xs.utils.test.chiselsim

import chisel3._
import chisel3.util._
import xs.utils.queue.MimoQueue
import xs.utils.test.WrapperTestPayload

import scala.collection.mutable
import scala.util.Random

private[test] case class MimoQueueCase(
  depth: Int,
  enqNum: Int,
  deqNum: Int,
  enqIsCompact: Boolean,
  width: Int,
  payload: String = "bundle"
)

private[test] case class MimoQueueLaneInput(valid: Boolean, bits: BigInt)
private[test] case class MimoQueueCycleInput(enq: Seq[MimoQueueLaneInput], deqReady: Seq[Boolean])

private[test] class MimoQueueTestPort(c: MimoQueueCase) extends Bundle {
  val enq = Flipped(Vec(c.enqNum, Decoupled(WrapperTestPayload(c.width, c.payload))))
  val deq = Vec(c.deqNum, Decoupled(WrapperTestPayload(c.width, c.payload)))
  val count = Output(UInt(log2Ceil(c.depth + 1).W))
}

// Keep aggregate ports through the entire harness; only the wrapper packs data.
private[test] class MimoQueueTestBank(cases: Seq[MimoQueueCase]) extends Module {
  val io = IO(MixedVec(cases.map(new MimoQueueTestPort(_))))
  cases.zip(io).foreach { case (c, port) =>
    val queue = Module(new MimoQueue(chiselTypeOf(port.enq.head.bits), c.enqNum, c.deqNum, c.depth, c.enqIsCompact))
    port <> queue.io
  }
}

private[test] class MimoQueueProducer(c: MimoQueueCase) {
  private val compactPending = mutable.Queue.empty[BigInt]
  private val sparsePending = Array.fill(c.enqNum)(Option.empty[BigInt])

  def pendingCount: Int = {
    if (c.enqIsCompact) compactPending.size else sparsePending.count(_.nonEmpty)
  }

  def add(value: BigInt): Int = {
    if (c.enqIsCompact) {
      require(compactPending.size < c.enqNum, s"compact producer has no free lane: $c")
      compactPending.enqueue(value)
      compactPending.size - 1
    } else {
      val lane = sparsePending.indexWhere(_.isEmpty)
      require(lane >= 0, s"sparse producer has no free lane: $c")
      sparsePending(lane) = Some(value)
      lane
    }
  }

  def addAt(lane: Int, value: BigInt): Unit = {
    require(!c.enqIsCompact, s"addAt requires a sparse producer: $c")
    require(lane >= 0 && lane < c.enqNum, s"lane $lane outside [0, ${c.enqNum})")
    require(sparsePending(lane).isEmpty, s"lane $lane is already valid: $c")
    sparsePending(lane) = Some(value)
  }

  def inputs: Seq[MimoQueueLaneInput] = {
    if (c.enqIsCompact) {
      Seq.tabulate(c.enqNum) { lane =>
        if (lane < compactPending.size) MimoQueueLaneInput(valid = true, compactPending(lane))
        else MimoQueueLaneInput(valid = false, BigInt(0))
      }
    } else {
      sparsePending.toSeq.map {
        case Some(value) => MimoQueueLaneInput(valid = true, value)
        case None => MimoQueueLaneInput(valid = false, BigInt(0))
      }
    }
  }

  def commit(fired: Seq[Boolean]): Unit = {
    require(fired.size == c.enqNum, s"fire vector width mismatch: $c")
    if (c.enqIsCompact) {
      val accepted = fired.count(identity)
      require(fired.take(accepted).forall(identity), s"compact accepts must be a prefix: $c $fired")
      require(fired.drop(accepted).forall(value => !value), s"compact accepts must be a prefix: $c $fired")
      for (_ <- 0 until accepted) compactPending.dequeue()
    } else {
      fired.zipWithIndex.foreach { case (isFired, lane) => if (isFired) sparsePending(lane) = None }
    }
  }
}

private[test] case class MimoQueuePlan(
  ready: Seq[Boolean],
  accepted: Seq[Boolean],
  popCount: Int
)

private[test] class MimoQueueModel(c: MimoQueueCase) {
  val entries = mutable.Queue.empty[BigInt]

  def count: Int = entries.size

  def expectedReady(valids: Seq[Boolean]): Seq[Boolean] = {
    val free = c.depth - count
    if (c.enqIsCompact) {
      Seq.tabulate(c.enqNum)(lane => free > lane)
    } else {
      var earlierValid = 0
      Seq.tabulate(c.enqNum) { lane =>
        val ready = free > earlierValid
        if (valids(lane)) earlierValid += 1
        ready
      }
    }
  }

  def plan(input: MimoQueueCycleInput): MimoQueuePlan = {
    require(input.enq.size == c.enqNum, s"enqueue width mismatch: $c")
    require(input.deqReady.size == c.deqNum, s"dequeue width mismatch: $c")
    val ready = expectedReady(input.enq.map(_.valid))
    val accepted = input.enq.indices.map(lane => input.enq(lane).valid && ready(lane))
    var pops = 0
    while (pops < c.deqNum && pops < count && input.deqReady(pops)) {
      pops += 1
    }
    MimoQueuePlan(ready, accepted, pops)
  }

  def commit(input: MimoQueueCycleInput, plan: MimoQueuePlan): Unit = {
    for (_ <- 0 until plan.popCount) entries.dequeue()
    input.enq.indices.foreach { lane =>
      if (plan.accepted(lane)) entries.enqueue(input.enq(lane).bits)
    }
  }
}

/** Run: xmake test -s MimoQueueSpec */
class MimoQueueSpec extends SimTestSpec {
  private val directedCases = Seq(
    MimoQueueCase(depth = 1, enqNum = 1, deqNum = 1, enqIsCompact = false, width = 8),
    MimoQueueCase(depth = 1, enqNum = 1, deqNum = 1, enqIsCompact = true, width = 8),
    MimoQueueCase(depth = 3, enqNum = 3, deqNum = 2, enqIsCompact = false, width = 8),
    MimoQueueCase(depth = 3, enqNum = 2, deqNum = 3, enqIsCompact = true, width = 8),
    MimoQueueCase(depth = 5, enqNum = 3, deqNum = 2, enqIsCompact = false, width = 8),
    MimoQueueCase(depth = 5, enqNum = 2, deqNum = 3, enqIsCompact = true, width = 8),
    MimoQueueCase(depth = 7, enqNum = 5, deqNum = 3, enqIsCompact = false, width = 8),
    MimoQueueCase(depth = 8, enqNum = 8, deqNum = 4, enqIsCompact = false, width = 8)
  )

  private val randomCases = Seq(
    MimoQueueCase(1, 1, 1, enqIsCompact = false, 7),
    MimoQueueCase(2, 2, 1, enqIsCompact = true, 7),
    MimoQueueCase(2, 1, 2, enqIsCompact = false, 7),
    MimoQueueCase(3, 3, 2, enqIsCompact = false, 7),
    MimoQueueCase(3, 2, 3, enqIsCompact = true, 7),
    MimoQueueCase(4, 4, 3, enqIsCompact = true, 7),
    MimoQueueCase(4, 2, 2, enqIsCompact = false, 7),
    MimoQueueCase(5, 3, 2, enqIsCompact = true, 7),
    MimoQueueCase(5, 2, 3, enqIsCompact = false, 7),
    MimoQueueCase(6, 4, 3, enqIsCompact = true, 7),
    MimoQueueCase(6, 3, 4, enqIsCompact = false, 7),
    MimoQueueCase(7, 2, 5, enqIsCompact = true, 7),
    MimoQueueCase(7, 5, 3, enqIsCompact = false, 7),
    MimoQueueCase(8, 8, 4, enqIsCompact = false, 7)
  )

  private val typedCases = for {
    (payload, width) <- Seq("bundle" -> 1, "bundle" -> 9, "bundle" -> 17,
      "bundle" -> 65, "vecBundle" -> 15, "vecBundle" -> 51, "vecBundle" -> 66)
    compact <- Seq(false, true)
  } yield MimoQueueCase(5, 3, 2, compact, width, payload)

  private def mask(c: MimoQueueCase, value: BigInt): BigInt = value & ((BigInt(1) << c.width) - 1)

  private def exerciseDirected(seed: Long): Unit = {
    info(s"seed=$seed directedConfigurations=${directedCases.size}")
    simulate(new MimoQueueTestBank(directedCases)) { dut =>
      val models = directedCases.map(new MimoQueueModel(_))
      var producers = directedCases.map(new MimoQueueProducer(_))
      var cycle = 0L
      var serial = 0L

      def fresh(i: Int): BigInt = {
        serial += 1
        mask(directedCases(i), (BigInt(i + 1) << 48) ^ BigInt(serial))
      }

      def clue(i: Int, phase: String, input: MimoQueueCycleInput): String =
        s"seed=$seed cycle=$cycle phase=$phase case=${directedCases(i)} count=${models(i).count} input=$input"

      def idleInput(i: Int): MimoQueueCycleInput =
        MimoQueueCycleInput(
          Seq.fill(directedCases(i).enqNum)(MimoQueueLaneInput(valid = false, BigInt(0))),
          Seq.fill(directedCases(i).deqNum)(false)
        )

      def checkStimulus(i: Int, input: MimoQueueCycleInput, phase: String): Unit = {
        val c = directedCases(i)
        val valid = input.enq.map(_.valid)
        if (c.enqIsCompact) {
          assert(valid.dropWhile(identity).forall(value => !value), s"${clue(i, phase, input)}: compact valids must be a prefix")
        }
        assert(input.deqReady.dropWhile(identity).forall(value => !value), s"${clue(i, phase, input)}: dequeue readies must be a prefix")
      }

      def poke(i: Int, input: MimoQueueCycleInput): Unit = {
        val c = directedCases(i)
        for (lane <- 0 until c.enqNum) {
          dut.io(i).enq(lane).valid.poke(input.enq(lane).valid.B)
          pokePayload(dut.io(i).enq(lane).bits, input.enq(lane).bits)
        }
        for (lane <- 0 until c.deqNum) dut.io(i).deq(lane).ready.poke(input.deqReady(lane).B)
      }

      def checkRegisteredOutputs(i: Int, phase: String, input: MimoQueueCycleInput): Unit = {
        val c = directedCases(i)
        val outputs = models(i).entries.toSeq
        val at = clue(i, phase, input)
        dut.io(i).count.expect(models(i).count.U, s"$at count")
        for (lane <- 0 until c.deqNum) {
          if (lane < outputs.size) {
            dut.io(i).deq(lane).valid.expect(true.B, s"$at deq valid lane $lane")
            expectPayload(dut.io(i).deq(lane).bits, outputs(lane), s"$at deq bits lane $lane")
          } else {
            dut.io(i).deq(lane).valid.expect(false.B, s"$at deq valid lane $lane")
          }
        }
      }

      def checkBeforeEdge(i: Int, input: MimoQueueCycleInput, phase: String): MimoQueuePlan = {
        checkStimulus(i, input, phase)
        poke(i, input)
        val plan = models(i).plan(input)
        val at = clue(i, phase, input)
        plan.ready.zipWithIndex.foreach { case (ready, lane) =>
          dut.io(i).enq(lane).ready.expect(ready.B, s"$at enq ready lane $lane")
        }
        checkRegisteredOutputs(i, phase, input)
        plan
      }

      def step(inputs: Seq[MimoQueueCycleInput], phase: String): Seq[MimoQueuePlan] = {
        require(inputs.size == directedCases.size, "directed input bank width mismatch")
        val plans = directedCases.indices.map(i => checkBeforeEdge(i, inputs(i), phase))
        dut.clock.step()
        plans.zipWithIndex.foreach { case (plan, i) => models(i).commit(inputs(i), plan) }
        cycle += 1
        directedCases.indices.foreach { i => checkRegisteredOutputs(i, s"$phase-after-edge", inputs(i)) }
        plans
      }

      def resetAll(): Unit = {
        directedCases.indices.foreach(i => poke(i, idleInput(i)))
        dut.reset.poke(true.B)
        dut.clock.step(2)
        models.foreach(_.entries.clear())
        producers = directedCases.map(new MimoQueueProducer(_))
        dut.reset.poke(false.B)
        cycle += 2
        directedCases.indices.foreach { i =>
          val input = idleInput(i)
          poke(i, input)
          val plan = models(i).plan(input)
          val at = clue(i, "reset", input)
          plan.ready.zipWithIndex.foreach { case (ready, lane) =>
            dut.io(i).enq(lane).ready.expect(ready.B, s"$at enq ready lane $lane")
          }
          checkRegisteredOutputs(i, "reset", input)
        }
      }

      def addUpTo(i: Int, limit: Int): Int = {
        var added = 0
        while (producers(i).pendingCount < limit) {
          producers(i).add(fresh(i))
          added += 1
        }
        added
      }

      def fillAll(): Unit = {
        while (directedCases.indices.exists(i => models(i).count < directedCases(i).depth)) {
          directedCases.indices.foreach { i =>
            val room = directedCases(i).depth - models(i).count
            if (room > 0) addUpTo(i, math.min(room, directedCases(i).enqNum))
          }
          val inputs = directedCases.indices.map(i => MimoQueueCycleInput(producers(i).inputs, Seq.fill(directedCases(i).deqNum)(false)))
          val plans = step(inputs, "fill")
          plans.zipWithIndex.foreach { case (plan, i) =>
            assert(plan.accepted.count(identity) == producers(i).inputs.count(_.valid), s"${clue(i, "fill", inputs(i))}: fill did not accept every offered lane")
            producers(i).commit(plan.accepted)
          }
        }
        directedCases.indices.foreach { i =>
          assert(models(i).count == directedCases(i).depth, s"case $i failed to fill")
        }
      }

      def drainAll(phase: String): Unit = {
        var guard = 0
        while (models.exists(_.count > 0)) {
          require(guard <= directedCases.map(_.depth).max + 2, s"$phase drain did not converge")
          val inputs = directedCases.indices.map { i =>
            MimoQueueCycleInput(
              Seq.fill(directedCases(i).enqNum)(MimoQueueLaneInput(valid = false, BigInt(0))),
              Seq.fill(directedCases(i).deqNum)(true)
            )
          }
          step(inputs, phase)
          guard += 1
        }
      }

      resetAll()

      // An empty queue must not forward a newly offered item before a clock edge.
      directedCases.indices.foreach(i => producers(i).add(fresh(i)))
      val emptyInputs = directedCases.indices.map { i =>
        MimoQueueCycleInput(producers(i).inputs, Seq.fill(directedCases(i).deqNum)(true))
      }
      val emptyPlans = step(emptyInputs, "empty-no-flow")
      emptyPlans.zipWithIndex.foreach { case (plan, i) =>
        assert(plan.popCount == 0, s"${clue(i, "empty-no-flow", emptyInputs(i))}: empty queue dequeued before registration")
        producers(i).commit(plan.accepted)
      }
      drainAll("empty-no-flow-drain")

      // Fill, then repeatedly consume the full output prefix and check the next
      // registered window after every edge. This catches stale replay bugs.
      fillAll()
      var consecutive = 0
      while (models.exists(_.count > 0)) {
        val before = models.map(_.count)
        val inputs = directedCases.indices.map { i =>
          MimoQueueCycleInput(
            Seq.fill(directedCases(i).enqNum)(MimoQueueLaneInput(valid = false, BigInt(0))),
            Seq.fill(directedCases(i).deqNum)(true)
          )
        }
        val plans = step(inputs, "consecutive-dequeue")
        plans.zipWithIndex.foreach { case (plan, i) =>
          val expectedPops = math.min(before(i), directedCases(i).deqNum)
          assert(plan.popCount == expectedPops, s"${clue(i, "consecutive-dequeue", inputs(i))}: expected $expectedPops prefix dequeues")
        }
        consecutive += 1
        require(consecutive <= directedCases.map(_.depth).max + 2, "consecutive dequeue did not drain")
      }

      // Sparse queues may accept gaps; compact queues group the remaining inputs.
      directedCases.indices.foreach { i =>
        val c = directedCases(i)
        if (c.enqIsCompact) {
          addUpTo(i, math.min(2, c.enqNum))
        } else if (c.enqNum >= 2) {
          producers(i).addAt(c.enqNum - 1, fresh(i))
          if (c.enqNum >= 3) producers(i).addAt(1, fresh(i))
        } else {
          producers(i).addAt(0, fresh(i))
        }
      }
      val groupedInputs = directedCases.indices.map(i => MimoQueueCycleInput(producers(i).inputs, Seq.fill(directedCases(i).deqNum)(false)))
      val groupedPlans = step(groupedInputs, "grouped-enqueue")
      groupedPlans.zipWithIndex.foreach { case (plan, i) =>
        assert(plan.accepted.count(identity) == groupedInputs(i).enq.count(_.valid), s"${clue(i, "grouped-enqueue", groupedInputs(i))}: grouped enqueue did not accept every offered lane")
        producers(i).commit(plan.accepted)
      }
      drainAll("grouped-drain")

      // At full occupancy, dequeue traffic must not create same-cycle enqueue
      // credit. Hold the rejected producer lanes stable, then accept a prefix
      // while a partial dequeue is completing.
      fillAll()
      directedCases.indices.foreach(i => addUpTo(i, directedCases(i).enqNum))
      val fullInputs = directedCases.indices.map { i =>
        val partialReady = math.max(1, directedCases(i).deqNum / 2)
        MimoQueueCycleInput(producers(i).inputs, Seq.tabulate(directedCases(i).deqNum)(lane => lane < partialReady))
      }
      val fullPlans = step(fullInputs, "full-backpressure")
      fullPlans.zipWithIndex.foreach { case (plan, i) =>
        assert(plan.accepted.forall(value => !value), s"${clue(i, "full-backpressure", fullInputs(i))}: full queue gave same-cycle dequeue credit")
        assert(plan.popCount == math.max(1, directedCases(i).deqNum / 2), s"${clue(i, "full-backpressure", fullInputs(i))}: full queue did not partially dequeue")
      }

      val beforePartial = models.map(_.count)
      val heldInputs = directedCases.indices.map { i =>
        val partialReady = math.max(1, directedCases(i).deqNum / 2)
        MimoQueueCycleInput(producers(i).inputs, Seq.tabulate(directedCases(i).deqNum)(lane => lane < partialReady))
      }
      val partialPlans = step(heldInputs, "partial-dequeue-simultaneous-enqueue")
      partialPlans.zipWithIndex.foreach { case (plan, i) =>
        assert(plan.accepted.exists(identity), s"${clue(i, "partial-dequeue-simultaneous-enqueue", heldInputs(i))}: available space was not used")
        val expectedPops = math.min(beforePartial(i), math.max(1, directedCases(i).deqNum / 2))
        assert(plan.popCount == expectedPops, s"${clue(i, "partial-dequeue-simultaneous-enqueue", heldInputs(i))}: partial dequeue did not complete")
        producers(i).commit(plan.accepted)
      }

      resetAll()

      // Sustained balanced traffic wraps the storage and registered windows for
      // many more entries than DEPTH, without changing occupancy.
      val wrapCycles = directedCases.map(_.depth).max * 4 + 8
      for (_ <- 0 until wrapCycles) {
        directedCases.indices.foreach { i =>
          val rate = math.min(directedCases(i).enqNum, directedCases(i).deqNum)
          addUpTo(i, rate)
        }
        val inputs = directedCases.indices.map { i =>
          val rate = math.min(directedCases(i).enqNum, directedCases(i).deqNum)
          MimoQueueCycleInput(producers(i).inputs, Seq.tabulate(directedCases(i).deqNum)(lane => lane < rate))
        }
        val plans = step(inputs, "wrap")
        plans.zipWithIndex.foreach { case (plan, i) => producers(i).commit(plan.accepted) }
      }
      directedCases.indices.foreach { i =>
        val rate = math.min(directedCases(i).enqNum, directedCases(i).deqNum)
        assert(models(i).count <= rate, s"case $i wrap occupancy exceeded offered rate: ${models(i).count} > $rate")
      }

      // Reset while data is queued must discard the registered window.
      fillAll()
      resetAll()
      directedCases.indices.foreach(i => assert(models(i).count == 0, s"case $i retained entries across reset"))
    }
  }

  private def exerciseRandom(cases: Seq[MimoQueueCase], defaultSeed: Long, defaultCycles: Int): Unit = {
    val seed = testSeed(defaultSeed)
    val cycles = testCycles(defaultCycles)
    info(s"seed=$seed cycles=$cycles configurations=${cases.size}")
    simulate(new MimoQueueTestBank(cases)) { dut =>
      val rng = new Random(seed)
      val models = cases.map(new MimoQueueModel(_))
      var producers = cases.map(new MimoQueueProducer(_))
      val serial = Array.fill(cases.size)(0L)
      var cycle = 0L

      def fresh(i: Int): BigInt = {
        serial(i) += 1
        mask(cases(i), (BigInt(i + 1) << 48) ^ BigInt(serial(i)))
      }

      def clue(i: Int, phase: String, input: MimoQueueCycleInput): String =
        s"seed=$seed cycle=$cycle phase=$phase case=${cases(i)} count=${models(i).count} input=$input"

      def checkStimulus(i: Int, input: MimoQueueCycleInput, phase: String): Unit = {
        val valid = input.enq.map(_.valid)
        if (cases(i).enqIsCompact) {
          assert(valid.dropWhile(identity).forall(value => !value), s"${clue(i, phase, input)}: compact valids must be a prefix")
        }
        assert(input.deqReady.dropWhile(identity).forall(value => !value), s"${clue(i, phase, input)}: dequeue readies must be a prefix")
      }

      def poke(i: Int, input: MimoQueueCycleInput): Unit = {
        val c = cases(i)
        for (lane <- 0 until c.enqNum) {
          dut.io(i).enq(lane).valid.poke(input.enq(lane).valid.B)
          pokePayload(dut.io(i).enq(lane).bits, input.enq(lane).bits)
        }
        for (lane <- 0 until c.deqNum) dut.io(i).deq(lane).ready.poke(input.deqReady(lane).B)
      }

      def checkOutputs(i: Int, phase: String, input: MimoQueueCycleInput): Unit = {
        val c = cases(i)
        val outputs = models(i).entries.toSeq
        val at = clue(i, phase, input)
        dut.io(i).count.expect(models(i).count.U, s"$at count")
        for (lane <- 0 until c.deqNum) {
          if (lane < outputs.size) {
            dut.io(i).deq(lane).valid.expect(true.B, s"$at deq valid lane $lane")
            expectPayload(dut.io(i).deq(lane).bits, outputs(lane), s"$at deq bits lane $lane")
          } else {
            dut.io(i).deq(lane).valid.expect(false.B, s"$at deq valid lane $lane")
          }
        }
      }

      def step(inputs: Seq[MimoQueueCycleInput], phase: String): Seq[MimoQueuePlan] = {
        val plans = cases.indices.map { i =>
          checkStimulus(i, inputs(i), phase)
          poke(i, inputs(i))
          val plan = models(i).plan(inputs(i))
          val at = clue(i, phase, inputs(i))
          plan.ready.zipWithIndex.foreach { case (ready, lane) =>
            dut.io(i).enq(lane).ready.expect(ready.B, s"$at enq ready lane $lane")
          }
          checkOutputs(i, phase, inputs(i))
          plan
        }
        dut.clock.step()
        plans.zipWithIndex.foreach { case (plan, i) => models(i).commit(inputs(i), plan) }
        cycle += 1
        cases.indices.foreach(i => checkOutputs(i, s"$phase-after-edge", inputs(i)))
        plans
      }

      def resetAll(): Unit = {
        cases.indices.foreach { i =>
          poke(i, MimoQueueCycleInput(
            Seq.fill(cases(i).enqNum)(MimoQueueLaneInput(valid = false, BigInt(0))),
            Seq.fill(cases(i).deqNum)(false)
          ))
        }
        dut.reset.poke(true.B)
        dut.clock.step(2)
        models.foreach(_.entries.clear())
        producers = cases.map(new MimoQueueProducer(_))
        dut.reset.poke(false.B)
        cycle += 2
      }

      resetAll()

      for (_ <- 0 until cycles) {
        cases.indices.foreach { i =>
          val c = cases(i)
          if (c.enqIsCompact) {
            while (producers(i).pendingCount < c.enqNum && rng.nextInt(3) == 0) {
              producers(i).add(fresh(i))
            }
          } else {
            for (lane <- 0 until c.enqNum) {
              val valid = producers(i).inputs(lane).valid
              if (!valid && rng.nextInt(3) == 0) producers(i).addAt(lane, fresh(i))
            }
          }
        }
        val inputs = cases.indices.map { i =>
          val readyCount = rng.nextInt(cases(i).deqNum + 1)
          MimoQueueCycleInput(producers(i).inputs, Seq.tabulate(cases(i).deqNum)(lane => lane < readyCount))
        }
        val plans = step(inputs, "random")
        plans.zipWithIndex.foreach { case (plan, i) => producers(i).commit(plan.accepted) }
      }

      // Drain the queue itself; pending producer values are intentionally no
      // longer offered. All ready lanes form a legal prefix.
      var guard = 0
      while (models.exists(_.count > 0)) {
        require(guard <= cases.map(_.depth).max + 2, "random drain did not converge")
        val inputs = cases.indices.map { i =>
          MimoQueueCycleInput(
            Seq.fill(cases(i).enqNum)(MimoQueueLaneInput(valid = false, BigInt(0))),
            Seq.fill(cases(i).deqNum)(true)
          )
        }
        step(inputs, "random-drain")
        guard += 1
      }
    }
  }

  private def exerciseThroughput(defaultCycles: Int): Unit = {
    val cycles = testCycles(defaultCycles)
    val fullCases = for {
      width <- Seq(1, 2, 3, 4)
      compact <- Seq(false, true)
    } yield MimoQueueCase(depth = width * 2, enqNum = width, deqNum = width, enqIsCompact = compact, width = 32)
    val asymmetricCases = Seq(
      MimoQueueCase(6, 3, 2, enqIsCompact = false, 32),
      MimoQueueCase(6, 3, 2, enqIsCompact = true, 32),
      MimoQueueCase(4, 2, 4, enqIsCompact = false, 32),
      MimoQueueCase(4, 2, 4, enqIsCompact = true, 32)
    )
    val cases = fullCases ++ asymmetricCases
    info(s"measuredCycles=$cycles fullThroughputConfigurations=${fullCases.size} asymmetricConfigurations=${asymmetricCases.size}")

    simulate(new MimoQueueTestBank(cases)) { dut =>
      val models = cases.map(new MimoQueueModel(_))
      val producers = cases.map(new MimoQueueProducer(_))
      val serial = Array.fill(cases.size)(0L)
      var cycle = 0L

      def fresh(i: Int): BigInt = {
        serial(i) += 1
        mask(cases(i), (BigInt(i + 1) << 40) ^ BigInt(serial(i)))
      }

      def clue(i: Int, phase: String): String =
        s"cycle=$cycle phase=$phase case=${cases(i)} count=${models(i).count}"

      def pokeAll(inputs: Seq[MimoQueueCycleInput]): Unit = {
        cases.indices.foreach { i =>
          for (lane <- 0 until cases(i).enqNum) {
            dut.io(i).enq(lane).valid.poke(inputs(i).enq(lane).valid.B)
            pokePayload(dut.io(i).enq(lane).bits, inputs(i).enq(lane).bits)
          }
          for (lane <- 0 until cases(i).deqNum) dut.io(i).deq(lane).ready.poke(inputs(i).deqReady(lane).B)
        }
      }

      def checkBeforeEdge(inputs: Seq[MimoQueueCycleInput]): Seq[MimoQueuePlan] = {
        val plans = cases.indices.map { i =>
          val plan = models(i).plan(inputs(i))
          plan.ready.zipWithIndex.foreach { case (ready, lane) =>
            dut.io(i).enq(lane).ready.expect(ready.B, s"${clue(i, "throughput")} enq ready lane $lane")
          }
          dut.io(i).count.expect(models(i).count.U, s"${clue(i, "throughput")} count")
          val outputs = models(i).entries.toSeq
          for (lane <- 0 until cases(i).deqNum) {
            if (lane < outputs.size) {
              dut.io(i).deq(lane).valid.expect(true.B, s"${clue(i, "throughput")} deq valid lane $lane")
              expectPayload(dut.io(i).deq(lane).bits, outputs(lane), s"${clue(i, "throughput")} deq bits lane $lane")
            } else {
              dut.io(i).deq(lane).valid.expect(false.B, s"${clue(i, "throughput")} deq valid lane $lane")
            }
          }
          plan
        }
        plans
      }

      def step(inputs: Seq[MimoQueueCycleInput], phase: String): Seq[MimoQueuePlan] = {
        pokeAll(inputs)
        val plans = checkBeforeEdge(inputs)
        dut.clock.step()
        plans.zipWithIndex.foreach { case (plan, i) => models(i).commit(inputs(i), plan) }
        cycle += 1
        cases.indices.foreach { i =>
          val outputs = models(i).entries.toSeq
          dut.io(i).count.expect(models(i).count.U, s"${clue(i, phase)}-after-edge count")
          for (lane <- 0 until cases(i).deqNum) {
            if (lane < outputs.size) {
              dut.io(i).deq(lane).valid.expect(true.B, s"${clue(i, phase)}-after-edge deq valid lane $lane")
              expectPayload(dut.io(i).deq(lane).bits, outputs(lane), s"${clue(i, phase)}-after-edge deq bits lane $lane")
            } else {
              dut.io(i).deq(lane).valid.expect(false.B, s"${clue(i, phase)}-after-edge deq valid lane $lane")
            }
          }
        }
        plans
      }

      def resetAll(): Unit = {
        val idle = cases.indices.map { i =>
          MimoQueueCycleInput(
            Seq.fill(cases(i).enqNum)(MimoQueueLaneInput(valid = false, BigInt(0))),
            Seq.fill(cases(i).deqNum)(false)
          )
        }
        pokeAll(idle)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        models.foreach(_.entries.clear())
        dut.reset.poke(false.B)
        cycle += 2
      }

      def addUpTo(i: Int, limit: Int): Unit = {
        while (producers(i).pendingCount < limit) producers(i).add(fresh(i))
      }

      def consume(inputs: Seq[MimoQueueCycleInput], phase: String, expectedRate: Seq[Int]): Unit = {
        val plans = step(inputs, phase)
        plans.zipWithIndex.foreach { case (plan, i) =>
          assert(plan.accepted.count(identity) == expectedRate(i), s"${clue(i, phase)}: enqueue rate ${plan.accepted.count(identity)} != ${expectedRate(i)}")
          assert(plan.popCount == expectedRate(i), s"${clue(i, phase)}: dequeue rate ${plan.popCount} != ${expectedRate(i)}")
          producers(i).commit(plan.accepted)
        }
      }

      resetAll()

      // Startup does not flow through the registered window, but once filled,
      // E=O=W at DEPTH=2W sustains W transfers in each direction every cycle.
      val fullStartup = cases.indices.map { i =>
        val rate = if (i < fullCases.size) cases(i).enqNum else 0
        addUpTo(i, rate)
        MimoQueueCycleInput(producers(i).inputs, Seq.tabulate(cases(i).deqNum)(lane => lane < rate))
      }
      val fullStartupPlans = step(fullStartup, "full-startup")
      fullStartupPlans.zipWithIndex.foreach { case (plan, i) =>
        val rate = if (i < fullCases.size) cases(i).enqNum else 0
        assert(plan.accepted.count(identity) == rate, s"${clue(i, "full-startup")}: enqueue did not fill")
        assert(plan.popCount == 0, s"${clue(i, "full-startup")}: empty queue flowed in the acceptance cycle")
        if (i < fullCases.size) producers(i).commit(plan.accepted)
      }

      for (_ <- 0 until cycles) {
        val inputs = cases.indices.map { i =>
          val rate = if (i < fullCases.size) cases(i).enqNum else 0
          addUpTo(i, rate)
          MimoQueueCycleInput(producers(i).inputs, Seq.tabulate(cases(i).deqNum)(lane => lane < rate))
        }
        consume(inputs, "full-throughput", cases.indices.map(i => if (i < fullCases.size) cases(i).enqNum else 0))
      }

      // Asymmetric port counts sustain only the bottleneck rate. This also
      // exercises legal ready prefixes when one side has extra ports.
      resetAll()
      val asymmetricOffset = fullCases.size
      val asymmetricStartup = cases.indices.map { i =>
        val rate = if (i >= asymmetricOffset) math.min(cases(i).enqNum, cases(i).deqNum) else 0
        addUpTo(i, rate)
        MimoQueueCycleInput(producers(i).inputs, Seq.tabulate(cases(i).deqNum)(lane => lane < rate))
      }
      val asymmetricStartupPlans = step(asymmetricStartup, "asymmetric-startup")
      asymmetricStartupPlans.zipWithIndex.foreach { case (plan, i) =>
        val rate = if (i >= asymmetricOffset) math.min(cases(i).enqNum, cases(i).deqNum) else 0
        assert(plan.accepted.count(identity) == rate, s"${clue(i, "asymmetric-startup")}: enqueue did not fill")
        assert(plan.popCount == 0, s"${clue(i, "asymmetric-startup")}: empty queue flowed in the acceptance cycle")
        if (i >= asymmetricOffset) producers(i).commit(plan.accepted)
      }

      for (_ <- 0 until cycles) {
        val inputs = cases.indices.map { i =>
          val rate = if (i >= asymmetricOffset) math.min(cases(i).enqNum, cases(i).deqNum) else 0
          addUpTo(i, rate)
          MimoQueueCycleInput(producers(i).inputs, Seq.tabulate(cases(i).deqNum)(lane => lane < rate))
        }
        consume(inputs, "asymmetric-throughput", cases.indices.map(i => if (i >= asymmetricOffset) math.min(cases(i).enqNum, cases(i).deqNum) else 0))
      }
    }
  }

  describe("MimoQueue registered multi-port output") {
    it("honors registered no-flow, stale replay, backpressure, reset, grouping, and wrap contracts") {
      exerciseDirected(testSeed(0x1a2b3c4dL))
    }

    it("matches an independent scoreboard under randomized protocol-correct traffic") {
      exerciseRandom(randomCases, 0x5eedL, defaultCycles = 240)
    }

    it("round-trips Bundle and Vec[Bundle] fields with compact and sparse inputs") {
      exerciseRandom(typedCases, 0x12345678L, defaultCycles = 80)
    }

    it("sustains full throughput at DEPTH=2W and the asymmetric bottleneck rate") {
      exerciseThroughput(defaultCycles = 160)
    }

    it("preserves lane order and registered behavior with two enqueue and three dequeue lanes") {
      val c = MimoQueueCase(depth = 5, enqNum = 2, deqNum = 3, enqIsCompact = false, width = 32)
      simulate(new MimoQueueTestBank(Seq(c))) { dut =>
        val port = dut.io.head
        val a = BigInt("89abcdef", 16)
        val b = BigInt("10203040", 16)
        val c = BigInt("fedcba98", 16)
        val d = BigInt("76543210", 16)
        val e = BigInt("d00dcafe", 16)
        val f = BigInt("13579bdf", 16)

        def drive(valid: Int, low: BigInt = 0, high: BigInt = 0, ready: Int = 0): Unit = {
          Seq(low, high).zipWithIndex.foreach { case (bits, lane) =>
            port.enq(lane).valid.poke(((valid >> lane) & 1).B)
            pokePayload(port.enq(lane).bits, bits)
          }
          for (lane <- 0 until 3) {
            port.deq(lane).ready.poke(((ready >> lane) & 1).B)
          }
        }

        def checkEnqReady(expected: Int): Unit = {
          for (lane <- 0 until 2) {
            port.enq(lane).ready.expect(((expected >> lane) & 1).B)
          }
        }

        def check(count: Int, head: BigInt*): Unit = {
          port.count.expect(count.U)
          for (lane <- 0 until 3) {
            port.deq(lane).valid.expect((lane < head.size).B)
          }
          if (head.nonEmpty) {
            head.zipWithIndex.foreach { case (value, lane) =>
              expectPayload(port.deq(lane).bits, value, s"dequeue lane $lane")
            }
          }
        }

        drive(0)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        check(0)
        checkEnqReady(3)

        // A sparse high input lane becomes output lane 0, but never flows before the edge.
        drive(2, low = b, high = a, ready = 7)
        checkEnqReady(3)
        check(0)
        dut.clock.step()
        check(1, a)

        drive(3, low = b, high = c)
        checkEnqReady(3)
        check(1, a)
        dut.clock.step()
        check(3, a, b, c)
        drive(3, low = d, high = e)
        checkEnqReady(3)
        check(3, a, b, c)
        dut.clock.step()
        check(5, a, b, c)
        drive(0)
        checkEnqReady(0)

        // Free one of five entries; sparse lane 1 can then use that single free slot.
        drive(0, ready = 1)
        check(5, a, b, c)
        dut.clock.step()
        check(4, b, c, d)
        drive(2, low = a, high = f, ready = 1)
        checkEnqReady(3)
        check(4, b, c, d)
        dut.clock.step()
        check(4, c, d, e)

        // Drain two lanes to expose the concurrently enqueued word in FIFO order.
        drive(0, ready = 3)
        check(4, c, d, e)
        dut.clock.step()
        check(2, e, f)

        // Parent reset must flush the wrapper's core, including an occupied output window.
        drive(0)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        check(0)
        checkEnqReady(3)
        dut.clock.step()
        check(0)
      }
    }

    it("rejects zero lanes and depths below the port count") {
      def expectInvalid(enqNum: Int, deqNum: Int, depth: Int): Unit = {
        val error = intercept[IllegalArgumentException] {
          simulate(new Module {
            val queue = Module(new MimoQueue(WrapperTestPayload(8), enqNum, deqNum, depth))
          }) { _ => () }
        }
        assert(error.getMessage.contains("MimoQueue"), s"unexpected validation error: ${error.getMessage}")
      }

      expectInvalid(enqNum = 0, deqNum = 1, depth = 1)
      expectInvalid(enqNum = 1, deqNum = 0, depth = 1)
      expectInvalid(enqNum = 2, deqNum = 1, depth = 1)
      expectInvalid(enqNum = 1, deqNum = 2, depth = 1)
    }
  }
}
