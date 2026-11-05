package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{Cat, MixedVec}
import circt.stage.ChiselStage
import xs.utils.SECDEDCode
import xs.utils.verilog._

import scala.util.Random

private[test] class SvSecdedPort(dataBits: Int) extends Bundle {
  private val codeBits = dataBits + SvSecded.eccBits(dataBits)
  val data = Input(UInt(dataBits.W))
  val receivedData = Input(UInt(dataBits.W))
  val receivedEcc = Input(UInt(SvSecded.eccBits(dataBits).W))
  val encoded = Output(UInt(codeBits.W))
  val decoded = Output(UInt(dataBits.W))
  val uncorrected = Output(UInt(dataBits.W))
  val ce = Output(Bool())
  val ue = Output(Bool())
  val encodersAgree = Output(Bool())
  val decodersAgree = Output(Bool())
}

private[test] class SvSecdedBank(widths: Seq[Int]) extends Module {
  val io = IO(MixedVec(widths.map(new SvSecdedPort(_))))
  widths.zip(io).foreach { case (width, port) =>
    val encoder = Module(new SvSecdedEncoder(width))
    val encoderExt = Module(new SvSecdedEncoderImpl(width))
    encoder.io.in := port.data
    encoderExt.io.in := port.data
    val encoded = SvSecdedEncoder(port.data)
    port.encoded := Cat(encoded.ecc, encoded.data)
    port.encodersAgree := (port.encoded === Cat(encoder.io.out.ecc, encoder.io.out.data)) &&
      (port.encoded === Cat(encoderExt.io.out.ecc, encoderExt.io.out.data)) && (port.encoded === new SECDEDCode().encode(port.data))

    val decoder = Module(new SvSecdedDecoder(width))
    val decoderExt = Module(new SvSecdedDecoderImpl(width))
    decoder.io.in.data := port.receivedData
    decoder.io.in.ecc := port.receivedEcc
    decoderExt.io.in.data := port.receivedData
    decoderExt.io.in.ecc := port.receivedEcc
    val decoded = SvSecdedDecoder(port.receivedData, port.receivedEcc)
    val reference = new SECDEDCode().decode(Cat(port.receivedEcc, port.receivedData))
    port.decoded := decoded.corrected
    port.uncorrected := decoded.uncorrected
    port.ce := decoded.ce
    port.ue := decoded.ue
    // The legacy decoder also flips data on double errors; the new decoder
    // deliberately gates correction. Compare its corrected data only otherwise.
    port.decodersAgree := (decoded.corrected === decoder.io.out) &&
      (decoded.corrected === decoderExt.io.out) &&
      (decoded.ce === decoder.io.ce) &&
      (decoded.ce === decoderExt.io.ce) &&
      (decoded.ue === decoder.io.ue) &&
      (decoded.ue === decoderExt.io.ue) &&
      (decoded.ce === reference.correctable) &&
      (decoded.ue === reference.uncorrectable) &&
      (decoded.ue || (decoded.corrected === reference.corrected))
  }
}

class SvSecdedSpec extends SimTestSpec {
  // Independent software encoder: place the payload into non-power-of-two
  // positions and XOR their indices to form all Hamming parity bits at once.
  private def encode(data: BigInt, width: Int): BigInt = {
    val r = SvSecded.eccBits(width) - 1
    val positions = (1 to width + r).filter(p => (p & (p - 1)) != 0)
    val parity = positions.zipWithIndex.filter { case (_, bit) => data.testBit(bit) }
      .map(_._1).foldLeft(0)(_ ^ _)
    val sec = (BigInt(parity) << width) | data
    sec | (BigInt(sec.bitCount & 1) << (width + r))
  }

  private def exercise(widths: Seq[Int], exhaustive: Boolean): Unit = {
    val seed = testSeed(0x5ecdedL)
    val rng = new Random(seed)
    val cycles = testCycles(16)
    info(s"seed=$seed randomPatterns=$cycles exhaustive=$exhaustive widths=${widths.mkString(",")}")
    simulate(new SvSecdedBank(widths)) { dut =>
      widths.zipWithIndex.foreach { case (width, index) =>
        val port = dut.io(index)
        val codeBits = width + SvSecded.eccBits(width)
        val dataMask = (BigInt(1) << width) - 1
        val values = if (exhaustive) (BigInt(0) to dataMask).toSeq
          else Seq(BigInt(0), dataMask, dataMask / 3) ++ Seq.fill(cycles)(BigInt(width, rng))
        values.zipWithIndex.foreach { case (data, pattern) =>
          val code = encode(data, width)
          val clue = s"width=$width data=0x${data.toString(16)}"
          port.data.poke(data.U)
          port.encoded.expect(code.U, clue)
          port.encodersAgree.expect(true.B, clue)
          def check(error: BigInt): Unit = {
            val received = code ^ error
            val double = error.bitCount == 2
            val context = s"$clue error=0x${error.toString(16)}"
            port.receivedData.poke((received & dataMask).U)
            port.receivedEcc.poke((received >> width).U)
            port.decoded.expect((if (double) received & dataMask else data).U, context)
            port.uncorrected.expect((received & dataMask).U, context)
            port.ce.expect((error.bitCount == 1).B, context)
            port.ue.expect(double.B, context)
            port.decodersAgree.expect(true.B, context)
          }
          check(BigInt(0))
          (0 until codeBits).foreach(bit => check(BigInt(1) << bit))
          // Exhaust all data/error combinations for small widths; for larger
          // widths exhaust every double-error pair on an alternating payload.
          if (exhaustive || pattern == 2) {
            for (a <- 0 until codeBits; b <- a + 1 until codeBits)
              check((BigInt(1) << a) | (BigInt(1) << b))
          } else {
            val a = rng.nextInt(codeBits)
            val b = (a + 1 + rng.nextInt(codeBits - 1)) % codeBits
            check((BigInt(1) << a) | (BigInt(1) << b))
          }
        }
      }
    }
  }

  describe("SvSecded encoder and decoder") {
    it("computes minimal ECC widths including overall parity without Int overflow") {
      val code = new SECDEDCode
      (1 to 4096).foreach { width =>
        val bits = SvSecded.eccBits(width)
        val r = bits - 1
        assert((BigInt(1) << r) >= width + r + 1)
        assert((BigInt(1) << (r - 1)) < width + r)
        assert(width + bits == code.width(width))
      }
      assert(SvSecded.eccBits(1) == 3)
      assert(SvSecded.eccBits(32) == 7)
      assert(SvSecded.eccBits(64) == 8)
      assert(SvSecded.eccBits(Int.MaxValue - 32) == 32)
      Seq(0, -1, Int.MaxValue, Int.MaxValue - 31).foreach { width =>
        intercept[IllegalArgumentException](SvSecded.eccBits(width))
      }
    }

    it("exhausts all payloads and zero/single/double errors for widths one through four") {
      exercise(1 to 4, exhaustive = true)
    }

    it("covers parity-width transitions, non-power-of-two payloads and every error-bit pair") {
      exercise(Seq(5, 8, 11, 12, 16, 26, 27, 32, 57, 58, 64, 120, 121, 128), exhaustive = false)
    }

    it("shares parameterized templates and discovers both modules for standalone export") {
      val rtl = ChiselStage.emitSystemVerilog(new SvSecdedBank(Seq(1, 32, 64)))
      for (name <- Seq("SvSecdedEncoder", "SvSecdedDecoder")) {
        val modules = ("(?m)^\\s*module\\s+(" + name + "(?:_[0-9]+)?)\\b").r
          .findAllMatchIn(rtl).map(_.group(1)).toSeq
        assert(modules == Seq(name), modules.mkString(", "))
        assert(SvModuleMain.discover().exists(_.moduleName == name))
      }
      assert(rtl.contains("parameter integer DATA_BITS = 64"))
      assert(rtl.contains("parameter integer ECC_BITS = 8"))
    }

    it("rejects invalid payload and ECC widths at elaboration") {
      for (width <- Seq(0, -1); ext <- Seq(false, true); encoder <- Seq(false, true)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            if (encoder && ext) { val bad = Module(new SvSecdedEncoderImpl(width)) }
            else if (encoder) { val bad = Module(new SvSecdedEncoder(width)) }
            else if (ext) { val bad = Module(new SvSecdedDecoderImpl(width)) }
            else { val bad = Module(new SvSecdedDecoder(width)) }
          })
        }
      }
      for (width <- Seq(0, 3, 5)) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val data = IO(Input(UInt(4.W)))
            val ecc = IO(Input(UInt(width.W)))
            val bad = SvSecdedDecoder(data, ecc)
          })
        }
      }
      intercept[IllegalArgumentException] {
        ChiselStage.emitCHIRRTL(new RawModule {
          val in = IO(Input(UInt(0.W)))
          val bad = SvSecdedEncoder(in)
        })
      }
    }
  }
}
