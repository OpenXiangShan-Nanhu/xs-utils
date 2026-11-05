package xs.utils.test.chiselsim

import chisel3._
import chisel3.simulator.scalatest.HasCliOptions.CliOption
import chisel3.simulator.scalatest.{ChiselSim, Cli}
import org.scalatest.funspec.AnyFunSpec
import xs.utils.test.WrapperTestPayload

/** Shared ChiselSim options. Concrete *Spec classes are discovered by Mill;
  * no per-suite registration in xmake.lua is required.
  */
abstract class SimTestSpec extends AnyFunSpec with ChiselSim with Cli.Simulator {
  addOption(CliOption.simple[Long]("seed", "Random seed (signed decimal or 0x hexadecimal)", value => {
    if (value.startsWith("0x") || value.startsWith("0X")) java.lang.Long.parseLong(value.drop(2), 16)
    else value.toLong
  }))
  addOption(CliOption.simple[Int]("cycles", "Positive random cycle count per configuration", value => {
    val cycles = value.toInt
    require(cycles > 0, "cycles must be positive")
    cycles
  }))

  // Drive/check each leaf independently; the test boundary stays aggregate typed.
  protected final def pokePayload(data: Data, bits: BigInt): Unit =
    WrapperTestPayload.fields(data, bits).foreach {
      case (signal: Bool, value, _) => signal.poke((value != 0).B)
      case (signal: UInt, value, _) => signal.poke(value.U)
      case (signal: SInt, value, _) => signal.poke(value.S)
      case (signal, _, _) => throw new IllegalArgumentException(s"unsupported leaf: $signal")
    }

  protected final def expectPayload(data: Data, bits: BigInt, clue: String = ""): Unit =
    WrapperTestPayload.fields(data, bits).foreach {
      case (signal: Bool, value, path) => signal.expect((value != 0).B, s"$clue $path")
      case (signal: UInt, value, path) => signal.expect(value.U, s"$clue $path")
      case (signal: SInt, value, path) => signal.expect(value.S, s"$clue $path")
      case (signal, _, _) => throw new IllegalArgumentException(s"unsupported leaf: $signal")
    }

  // Options are available only inside an it(...) body; never cache at construction.
  protected final def testSeed(default: Long = 0x5eedL): Long = getOption[Long]("seed").getOrElse(default)
  protected final def testCycles(default: Int = 1000): Int = {
    require(default > 0, "default cycles must be positive")
    getOption[Int]("cycles").getOrElse(default)
  }
}
