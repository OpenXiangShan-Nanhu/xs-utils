package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{MixedVec, Valid}
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import xs.utils.GlobalData
import xs.utils.verilog.{SvTreePLRU, SvTreePLRUImpl}

import scala.util.Random

private[test] class TreePLRUPort(size: Int) extends Bundle {
  val touch = Input(Valid(UInt(size.W)))
  val results = Output(Vec(if (size == 1) 3 else 4, UInt(size.W)))
}

private[test] class TreePLRUBank(sizes: Seq[Int]) extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  val io = IO(MixedVec(sizes.map(new TreePLRUPort(_))))

  sizes.zip(io).foreach { case (size, port) =>
    val wrapper = Module(new SvTreePLRU(size))
    wrapper.io.touch := port.touch

    port.results(0) := wrapper.io.out
    port.results(1) := SvTreePLRU(port.touch)
    port.results(2) := SvTreePLRU(port.touch.bits, port.touch.valid)
    if (size > 1) {
      val core = Module(new SvTreePLRUImpl(size))
      core.clock := clock
      core.reset := reset.asAsyncReset
      core.io.touch := port.touch
      port.results(3) := core.io.out
    }
  }
}

class SvTreePLRUSpec extends SimTestSpec {
  // Independent tree oracle: traverse the heap in order to assign way numbers.
  // Updates walk from the touched leaf toward the root, unlike the RTL's OR tree.
  private class Reference(val size: Int) {
    private val leafWay = Array.fill(2 * size - 1)(-1)
    private val wayLeaf = Array.fill(size)(-1)
    private val olderRight = Array.fill(size - 1)(false)
    private var nextWay = 0

    private def enumerate(node: Int): Unit = {
      if (node >= size - 1) {
        leafWay(node) = nextWay
        wayLeaf(nextWay) = node
        nextWay += 1
      } else {
        enumerate(2 * node + 1)
        enumerate(2 * node + 2)
      }
    }
    enumerate(0)

    def reset(): Unit = java.util.Arrays.fill(olderRight, false)
    def victim: Int = {
      var node = 0
      while (node < size - 1) node = 2 * node + (if (olderRight(node)) 2 else 1)
      leafWay(node)
    }
    def victimOH: BigInt = BigInt(1) << victim

    def touch(way: Int): Unit = {
      var node = wayLeaf(way)
      while (node > 0) {
        val parent = (node - 1) / 2
        olderRight(parent) = (node & 1) == 1
        node = parent
      }
    }
  }

  private def exercise(sizes: Seq[Int]): Unit = {
    val seed = testSeed(0x71eeL)
    val cycles = testCycles(120)
    val rng = new Random(seed)
    val references = sizes.map(new Reference(_))
    info(s"sizes=${sizes.mkString(",")} seed=$seed cycles=$cycles")

    simulate(new TreePLRUBank(sizes)) { dut =>
      def check(phase: String): Unit = {
        sizes.zip(dut.io).zip(references).foreach { case ((size, port), ref) =>
          val expected = ref.victimOH
          assert(expected.bitCount == 1 && expected < (BigInt(1) << size))
          port.results.zipWithIndex.foreach { case (result, path) =>
            result.expect(expected.U, s"size=$size phase=$phase path=$path seed=$seed")
          }
        }
      }

      dut.io.foreach { port =>
        port.touch.valid.poke(false.B)
        port.touch.bits.poke(0.U)
      }
      dut.reset.poke(true.B)
      dut.clock.step()
      check("initial reset: way zero")
      dut.reset.poke(false.B)

      val directed = sizes.max * 2 + 12
      for (cycle <- 0 until directed + cycles) {
        if (cycle == directed + cycles / 2) {
          // A second reset must discard previous touches even during active input.
          dut.io.foreach { port =>
            port.touch.valid.poke(true.B)
            port.touch.bits.poke(1.U)
          }
          dut.reset.poke(true.B)
          dut.clock.step()
          references.foreach(_.reset())
          check(s"mid-stream reset at cycle=$cycle")
          dut.reset.poke(false.B)
        }

        val touches = sizes.map { size =>
          if (cycle < sizes.max * 2) (true, BigInt(1) << (cycle % size))
          else if (cycle < directed) (true, BigInt(1) << (size - 1))
          else if (cycle % 11 == 0) (true, BigInt(0)) // Valid zero is a no-op.
          else if (cycle % 7 == 0) (false, (BigInt(1) << size) - 1) // Disabled touch is a no-op.
          else (true, BigInt(1) << rng.nextInt(size))
        }
        dut.io.zip(touches).foreach { case (port, (valid, mask)) =>
          port.touch.valid.poke(valid.B)
          port.touch.bits.poke(mask.U)
        }
        check(s"before edge $cycle")
        dut.clock.step()
        references.zip(touches).foreach { case (ref, (valid, mask)) =>
          if (valid && mask != 0) ref.touch(mask.bigInteger.getLowestSetBit)
        }
        check(s"after edge $cycle")
      }
    }
  }

  describe("SvTreePLRU") {
    it("tracks an independent one-hot oracle across singleton, odd and power-of-two trees") {
      exercise(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    it("keeps outputs valid across large non-power-of-two widths and random touches") {
      exercise(Seq(15, 16, 17, 33, 65))
    }

    it("inlines the wrapper and registers one prefixed parameterized SV template") {
      GlobalData.synchronized {
        val saved = GlobalData.prefix
        try for (prefix <- Seq("", "soc_")) {
          GlobalData.prefix = prefix
          val name = prefix + "SvTreePLRU"
          val annotations = ChiselStage.elaborate(new TreePLRUBank(Seq(1, 3, 8))).annotations.collect {
            case a: BlackBoxInlineAnno => a.name
          }.toSeq
          assert(annotations == Seq(name + ".sv"))
          val rtl = ChiselStage.emitSystemVerilog(new TreePLRUBank(Seq(1, 3, 8)))
          val names = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
          assert(names.sorted == Seq("TreePLRUBank", name).sorted)
          assert(Seq(3, 8).forall(size => rtl.contains(s".INPUT_SIZE($size)")))
          assert(!rtl.contains(".INPUT_SIZE(1)"))
        } finally GlobalData.prefix = saved
      }
    }

    it("bypasses the SV core for a single way in the wrapper and both factories") {
      val annotations = ChiselStage.elaborate(new TreePLRUBank(Seq(1))).annotations.collect {
        case a: BlackBoxInlineAnno => a.name
      }.toSeq
      assert(annotations.isEmpty)
      val rtl = ChiselStage.emitSystemVerilog(new TreePLRUBank(Seq(1)))
      val names = "(?m)^\\s*module\\s+(\\w+)".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
      assert(names == Seq("TreePLRUBank"))
    }

    it("rejects a single way in the Impl, nonpositive input sizes and invalid factory inputs") {
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module {
          val invalid = Module(new SvTreePLRUImpl(1))
        })
      }
      for (size <- Seq(0, -1); ext <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new Module {
            if (ext) { val invalid = Module(new SvTreePLRUImpl(size)) }
            else { val invalid = Module(new SvTreePLRU(size)) }
          })
        }
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new Module {
          val touch = Wire(Valid(UInt()))
          val invalid = SvTreePLRU(touch)
        })
      }
    }
  }
}
