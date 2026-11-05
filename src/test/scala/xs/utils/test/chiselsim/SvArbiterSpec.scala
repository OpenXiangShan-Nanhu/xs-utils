package xs.utils.test.chiselsim

import chisel3._
import chisel3.util.{Arbiter => NativeArbiter, ArbiterIO, Decoupled, DecoupledIO, MixedVec, log2Ceil}
import circt.stage.ChiselStage
import firrtl.transforms.BlackBoxInlineAnno
import xs.utils.GlobalData
import xs.utils.test.{WrapperTestPacket, WrapperTestPayload}
import xs.utils.verilog.{SvArbiter, SvArbiterImpl, SvMux1H, SvMux1HImpl, VlogRegistry}

import scala.util.Random

private[test] case class SvArbiterCase(width: Int, size: Int, payload: String = "bundle") {
  require(size > 0, "SvArbiter test needs at least one input")
  def gen: Data = WrapperTestPayload(width, payload)
}

private[test] class SvArbiterInput(gen: Data) extends Bundle {
  val valid = Bool()
  val bits = gen
}

private[test] class SvArbiterPort(c: SvArbiterCase) extends Bundle {
  private def chosenWidth = math.max(log2Ceil(c.size), 1)
  val in = Input(Vec(c.size, new SvArbiterInput(c.gen)))
  val actualOut = Decoupled(c.gen)
  val factoryOut = Decoupled(c.gen)
  val nativeOut = Decoupled(c.gen)
  val actualReady = Output(UInt(c.size.W))
  val factoryReady = Output(UInt(c.size.W))
  val nativeReady = Output(UInt(c.size.W))
  val actualChosen = Output(UInt(chosenWidth.W))
  val factoryChosen = Output(UInt(chosenWidth.W))
  val nativeChosen = Output(UInt(chosenWidth.W))
}

private[test] class SvArbiterBank(cases: Seq[SvArbiterCase]) extends Module {
  val io = IO(MixedVec(cases.map(new SvArbiterPort(_))))

  cases.zip(io).foreach { case (c, port) =>
    val gen = chiselTypeOf(port.in.head.bits)
    val actual = Module(new SvArbiter(gen, c.size))
    val factory = SvArbiter(chiselTypeOf(port.in.head.bits), c.size)
    val native = Module(new NativeArbiter(chiselTypeOf(port.in.head.bits), c.size))

    def connect(arb: ArbiterIO[Data], out: DecoupledIO[Data], ready: UInt, chosen: UInt): Unit = {
      for (i <- 0 until c.size) {
        arb.in(i).valid := port.in(i).valid
        arb.in(i).bits := port.in(i).bits
      }
      out.valid := arb.out.valid
      out.bits := arb.out.bits
      arb.out.ready := out.ready
      ready := VecInit(arb.in.map(_.ready)).asUInt
      chosen := arb.chosen
    }

    connect(actual.io, port.actualOut, port.actualReady, port.actualChosen)
    connect(factory, port.factoryOut, port.factoryReady, port.factoryChosen)
    connect(native.io, port.nativeOut, port.nativeReady, port.nativeChosen)
  }
}

/** Structurally matches WrapperTestPacket, but widths must flow through the bypass. */
private[test] class SvArbiterInferredPayload extends Bundle {
  val tag = UInt()
  val delta = SInt()
  val flags = Vec(3, Bool())
}

/** Deliberately contains no raw core or native reference to mask source registration. */
private[test] class SvArbiterOneRtlTop(c: SvArbiterCase, factory: Boolean, inferWidths: Boolean = false)
    extends RawModule {
  require(c.size == 1)
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(c.gen))
    val out = Decoupled(c.gen)
    val chosen = Output(UInt(1.W))
  })

  private val gen = if (inferWidths) {
    val inferred: Data = if (c.payload == "vecBundle") Vec(3, new SvArbiterInferredPayload)
      else new SvArbiterInferredPayload
    require(!inferred.isWidthKnown, "this fixture must exercise unknown payload widths")
    inferred
  } else chiselTypeOf(io.in.bits)
  private val arbiter = if (factory) SvArbiter(gen, 1) else Module(new SvArbiter(gen, 1)).io
  require(arbiter.chosen.getWidth == 0, "the public ArbiterIO must retain native n=1 chosen width")
  arbiter.in.head.valid := io.in.valid
  arbiter.in.head.bits := io.in.bits
  io.in.ready := arbiter.in.head.ready
  io.out.valid := arbiter.out.valid
  io.out.bits := arbiter.out.bits
  arbiter.out.ready := io.out.ready
  io.chosen := arbiter.chosen
}

/** Test the explicit raw INPUT_SIZE=1 boundary separately from bypass-only fixtures. */
private[test] class SvArbiterRawOneTop extends RawModule {
  val in = IO(Flipped(Decoupled(new WrapperTestPacket(17))))
  val out = IO(Decoupled(new WrapperTestPacket(17)))
  val chosen = IO(Output(UInt(1.W)))
  private val core = Module(new SvArbiterImpl(17, 1))
  core.io.in.valid := in.valid.asUInt
  core.io.in.bits := in.bits.asUInt
  in.ready := core.io.in.ready(0)
  out.valid := core.io.out.valid
  out.bits := core.io.out.bits.asTypeOf(out.bits)
  core.io.out.ready := out.ready
  chosen := core.io.chosen
}

private[test] class SvArbiterRegistryTop extends Module {
  private val gen = new WrapperTestPacket(9)
  val io = IO(new Bundle {
    val in = Input(Vec(2, new SvArbiterInput(gen)))
    val wrappedOut = Decoupled(gen)
    val wrappedReady = Output(UInt(2.W))
    val wrappedChosen = Output(UInt(1.W))
    val directOut = Decoupled(gen)
    val directReady = Output(UInt(2.W))
    val directChosen = Output(UInt(1.W))
  })

  private val wrapped = SvArbiter(chiselTypeOf(io.in.head.bits), 2)
  for (i <- 0 until 2) {
    wrapped.in(i).valid := io.in(i).valid
    wrapped.in(i).bits := io.in(i).bits
  }
  io.wrappedReady := VecInit(wrapped.in.map(_.ready)).asUInt
  io.wrappedOut <> wrapped.out
  io.wrappedChosen := wrapped.chosen

  private val direct = Module(new SvArbiterImpl(9, 2))
  direct.io.in.valid := VecInit(io.in.map(_.valid)).asUInt
  direct.io.in.bits := VecInit(io.in.map(_.bits)).asUInt
  io.directReady := direct.io.in.ready
  io.directOut.valid := direct.io.out.valid
  io.directOut.bits := direct.io.out.bits.asTypeOf(io.directOut.bits)
  direct.io.out.ready := io.directOut.ready
  io.directChosen := direct.io.chosen
}

/** Direct typed/raw muxes and an arbiter-internal SV mux share one definition. */
private[test] class SvArbiterMuxDependencyTop(muxFirst: Boolean) extends RawModule {
  private val gen = Vec(3, new WrapperTestPacket(17))
  val io = IO(new Bundle {
    val in = Input(Vec(3, new SvArbiterInput(gen)))
    val muxSel = Input(UInt(3.W))
    val typedMuxOut = Output(gen.cloneType)
    val rawMuxOut = Output(gen.cloneType)
    val arbiterOut = Decoupled(gen.cloneType)
    val ready = Output(UInt(3.W))
    val chosen = Output(UInt(2.W))
  })

  private def connectMuxes(): Unit = {
    val typed = Module(new SvMux1H(chiselTypeOf(io.in.head.bits), 3))
    typed.io.sel := io.muxSel
    typed.io.in := VecInit(io.in.map(_.bits))
    io.typedMuxOut := typed.io.out
    val direct = Module(new SvMux1HImpl(gen.getWidth, 3))
    direct.io.sel := io.muxSel
    direct.io.in := VecInit(io.in.map(_.bits)).asUInt
    io.rawMuxOut := direct.io.out.asTypeOf(io.rawMuxOut)
  }

  private def connectArbiter(): Unit = {
    val arbiter = SvArbiter(chiselTypeOf(io.in.head.bits), 3)
    arbiter.in.zip(io.in).foreach { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    io.ready := VecInit(arbiter.in.map(_.ready)).asUInt
    io.chosen := arbiter.chosen
    io.arbiterOut <> arbiter.out
  }

  if (muxFirst) {
    connectMuxes()
    connectArbiter()
  } else {
    connectArbiter()
    connectMuxes()
  }
}

class SvArbiterSpec extends SimTestSpec {
  private def withPrefix[T](prefix: String)(body: => T): T = GlobalData.synchronized {
    val previous = GlobalData.prefix
    GlobalData.prefix = prefix
    try body finally GlobalData.prefix = previous
  }

  private def lowestValid(mask: BigInt, size: Int): Int = {
    val index = (0 until size).find(mask.testBit)
    index.getOrElse(size - 1)
  }

  private def exercise(cases: Seq[SvArbiterCase]): Unit = {
    val seed = testSeed(0xa4b1L)
    val randomCycles = testCycles(24)
    val rng = new Random(seed)
    info(s"seed=$seed randomCycles=$randomCycles configurations=${cases.size}")
    simulate(new SvArbiterBank(cases)) { dut =>
      cases.zipWithIndex.foreach { case (c, caseIndex) =>
        val port = dut.io(caseIndex)
        val masks = (BigInt(0) until (BigInt(1) << c.size)) ++
          Seq.fill(randomCycles)(BigInt(c.size, rng))

        masks.zipWithIndex.foreach { case (mask, cycle) =>
          val data = Seq.tabulate(c.size)(_ => BigInt(c.width, rng))
          val chosen = lowestValid(mask, c.size)
          val expectedBits = data(chosen)
          val expectedValid = mask != 0
          val outReady = (cycle & 1) == 0

          for (i <- 0 until c.size) {
            port.in(i).valid.poke(mask.testBit(i).B)
            pokePayload(port.in(i).bits, data(i))
          }
          port.actualOut.ready.poke(outReady.B)
          port.factoryOut.ready.poke(outReady.B)
          port.nativeOut.ready.poke(outReady.B)

          val expectedReady = if (outReady) (BigInt(1) << (chosen + 1)) - 1 else BigInt(0)
          val clue = s"seed=$seed case=$c cycle=$cycle mask=0x${mask.toString(16)} ready=$outReady"

          Seq(port.actualOut, port.factoryOut, port.nativeOut).foreach { out =>
            out.valid.expect(expectedValid.B, clue)
            expectPayload(out.bits, expectedBits, clue)
          }
          Seq(port.actualReady, port.factoryReady, port.nativeReady).foreach {
            _.expect(expectedReady.U, clue)
          }
          Seq(port.actualChosen, port.factoryChosen, port.nativeChosen).foreach {
            _.expect(chosen.U, clue)
          }
        }
      }
    }
  }

  private def exerciseBypass(c: SvArbiterCase, factory: Boolean, inferWidths: Boolean = false): Unit = {
    val rng = new Random(testSeed(0xb1a55L))
    val values = Seq(BigInt(0), (BigInt(1) << c.width) - 1) ++
      (0 until c.width).map(bit => BigInt(1) << bit) ++
      Seq.fill(testCycles(16))(BigInt(c.width, rng))
    simulateRaw(new SvArbiterOneRtlTop(c, factory, inferWidths)) { dut =>
      for (bits <- values; valid <- Seq(false, true); ready <- Seq(false, true)) {
        val clue = s"case=$c factory=$factory inferred=$inferWidths valid=$valid ready=$ready bits=$bits"
        pokePayload(dut.io.in.bits, bits)
        dut.io.in.valid.poke(valid.B)
        dut.io.out.ready.poke(ready.B)
        dut.io.out.valid.expect(valid.B, clue)
        dut.io.in.ready.expect(ready.B, clue)
        dut.io.chosen.expect(0.U, clue)
        expectPayload(dut.io.out.bits, bits, clue)
      }
    }
  }

  describe("SvArbiter") {
    it("matches native Arbiter and a software reference for Bundle and Vec[Bundle] payloads") {
      exercise(Seq(
        SvArbiterCase(1, 1),
        SvArbiterCase(5, 3),
        SvArbiterCase(17, 5),
        SvArbiterCase(51, 7, "vecBundle"),
        SvArbiterCase(66, 4, "vecBundle")
      ))
    }

    it("keeps all idle lanes ready, masks ready under backpressure, and gives lane zero priority") {
      val c = SvArbiterCase(17, 5)
      simulate(new SvArbiterBank(Seq(c))) { dut =>
        val port = dut.io(0)
        val probes = Seq(
          (BigInt(0), true),
          (BigInt(0), false),
          (BigInt(1), true),
          (BigInt(2), true),
          (BigInt("10110", 2), true),
          (BigInt("11111", 2), true),
          (BigInt("11111", 2), false)
        )
        probes.zipWithIndex.foreach { case ((mask, outReady), cycle) =>
          val data = Seq.tabulate(c.size)(lane => BigInt(c.width, new Random(cycle * 31L + lane)))
          for (i <- 0 until c.size) {
            port.in(i).valid.poke(mask.testBit(i).B)
            pokePayload(port.in(i).bits, data(i))
          }
          port.actualOut.ready.poke(outReady.B)
          port.factoryOut.ready.poke(outReady.B)
          port.nativeOut.ready.poke(outReady.B)
          val chosen = lowestValid(mask, c.size)
          val expectedReady = if (outReady) (BigInt(1) << (chosen + 1)) - 1 else BigInt(0)
          val clue = s"mask=0x${mask.toString(16)} ready=$outReady"
          Seq(port.actualReady, port.factoryReady, port.nativeReady).foreach {
            _.expect(expectedReady.U, clue)
          }
          Seq(port.actualChosen, port.factoryChosen, port.nativeChosen).foreach {
            _.expect(chosen.U, clue)
          }
          Seq(port.actualOut, port.factoryOut, port.nativeOut).foreach { out =>
            out.valid.expect((mask != 0).B, clue)
            expectPayload(out.bits, data(chosen), clue)
          }
        }
      }
    }

    it("inlines multi-input wrappers and deduplicates direct ExtModules with global prefixes") {
      val bankRtl = ChiselStage.emitSystemVerilog(new SvArbiterBank(Seq(
        SvArbiterCase(8, 2),
        SvArbiterCase(9, 3),
        SvArbiterCase(12, 4)
      )))
      val bankModules = "(?m)^\\s*module\\s+(\\w+)\\b".r.findAllMatchIn(bankRtl).map(_.group(1)).toSeq
      assert(bankModules.count(_ == "SvArbiter") == 1, bankModules.mkString(", "))
      assert("(?m)^\\s*SvArbiter\\s*#\\(".r.findAllIn(bankRtl).size >= 6)
      assert(bankRtl.contains("parameter integer DATA_WIDTH"))
      assert(bankRtl.contains("parameter integer INPUT_SIZE"))

      val registryRtl = ChiselStage.emitSystemVerilog(new SvArbiterRegistryTop)
      val registryModules = "(?m)^\\s*module\\s+(\\w+)\\b".r.findAllMatchIn(registryRtl).map(_.group(1)).toSeq
      assert(registryModules.count(_ == "SvArbiter") == 1, registryModules.mkString(", "))
      assert("(?m)^\\s*SvArbiter\\s*#\\(".r.findAllIn(registryRtl).size >= 2)

      val prefixedRtl = withPrefix("soc_") { ChiselStage.emitSystemVerilog(new SvArbiterRegistryTop) }
      val prefixedModules = "(?m)^\\s*module\\s+(\\w+)\\b".r.findAllMatchIn(prefixedRtl).map(_.group(1)).toSeq
      assert(prefixedModules.count(_ == "soc_SvArbiter") == 1, prefixedModules.mkString(", "))
      assert("(?m)^\\s*soc_SvArbiter\\s*#\\(".r.findAllIn(prefixedRtl).size >= 2)

    }

    it("registers the prefixed encoder and mux dependency closure without a Chisel helper instance") {
      for (prefix <- Seq("", "soc_", "chip_")) withPrefix(prefix) {
        val registry = new VlogRegistry
        val sources = registry.sources(SvArbiterImpl)
        val expectedNames = Seq(s"${prefix}SvPriorityEncoder", s"${prefix}SvMux1H", s"${prefix}SvArbiter")
        assert(sources.map(_.moduleName) == expectedNames)
        assert(sources.map(_.fileName) == expectedNames.map(_ + ".sv"))
        assert(sources == registry.sources(SvArbiterImpl))
        val arbiterSource = sources.last.text
        assert(arbiterSource.contains(s"${prefix}SvPriorityEncoder #(.INPUT_SIZE(INPUT_SIZE)) u_choice"))
        assert(arbiterSource.contains(
          s"${prefix}SvMux1H #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(INPUT_SIZE)) u_out_bits"))
        assert(arbiterSource.contains(".io_sel(chosen_oh)"))
        assert(arbiterSource.contains(".io_in(io_in_bits)"))
        assert(arbiterSource.contains(".io_out(io_out_bits)"))
        assert(!arbiterSource.contains("selected_term") && !arbiterSource.contains("selected_red"))

        val elaborated = ChiselStage.elaborate(new SvArbiterRegistryTop)
        val attachments = elaborated.annotations.collect { case a: BlackBoxInlineAnno => a }.toSeq
        assert(attachments.map(_.name).sorted == expectedNames.map(_ + ".sv").sorted)
        val extmodules = """(?m)^\s*extmodule\s+(\w+)""".r
          .findAllMatchIn(elaborated.serialize).map(_.group(1)).toSeq
        assert(extmodules.nonEmpty && extmodules.forall(_.startsWith(s"${prefix}SvArbiter")),
          extmodules.mkString(", "))
      }
    }

    it("emits one SvMux1H source for direct muxes and SV arbiter dependencies in either order") {
      for (muxFirst <- Seq(false, true); prefix <- Seq("", "soc_")) withPrefix(prefix) {
        val clue = s"muxFirst=$muxFirst prefix=$prefix"
        val elaborated = ChiselStage.elaborate(new SvArbiterMuxDependencyTop(muxFirst))
        val attachments = elaborated.annotations.collect { case a: BlackBoxInlineAnno => a }.toSeq
        val expectedFiles = Seq(s"${prefix}SvArbiter.sv", s"${prefix}SvMux1H.sv", s"${prefix}SvPriorityEncoder.sv")
        assert(attachments.map(_.name).sorted == expectedFiles.sorted, clue)
        val rtl = ChiselStage.emitSystemVerilog(new SvArbiterMuxDependencyTop(muxFirst))
        val modules = """(?m)^\s*module\s+(\w+)\b""".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
        assert(modules.toSet == Set("SvArbiterMuxDependencyTop", s"${prefix}SvArbiter", s"${prefix}SvMux1H", s"${prefix}SvPriorityEncoder"),
          s"$clue modules=$modules")
        assert(modules.size == 4, s"$clue modules=$modules")
        val muxInstance = ("""(?m)^\s*""" + prefix + """SvMux1H\s*#\s*\(""").r
        val topBody = rtl.substring(rtl.indexOf("module SvArbiterMuxDependencyTop")).split("endmodule")(0)
        val arbiterBody = rtl.substring(rtl.indexOf(s"module ${prefix}SvArbiter #(")).split("endmodule")(0)
        assert(muxInstance.findFirstIn(topBody).nonEmpty, s"$clue direct mux missing")
        assert(muxInstance.findAllIn(arbiterBody).size == 1, s"$clue nested mux missing or duplicated")
        assert(arbiterBody.contains("u_out_bits"), clue)
      }
    }

    it("simulates direct typed and raw muxes alongside the SV arbiter output mux") {
      val rng = new Random(testSeed(0x51a4bL))
      for (muxFirst <- Seq(false, true)) {
        simulateRaw(new SvArbiterMuxDependencyTop(muxFirst)) { dut =>
          for (requestMask <- 0 until 8; selectMask <- 0 until 8; ready <- Seq(false, true)) {
            val data = Seq.fill(3)(BigInt(51, rng))
            val chosen = lowestValid(BigInt(requestMask), 3)
            val muxBits = data.zipWithIndex.collect {
              case (bits, lane) if (selectMask & (1 << lane)) != 0 => bits
            }.foldLeft(BigInt(0))(_ | _)
            val clue = s"muxFirst=$muxFirst requests=$requestMask select=$selectMask ready=$ready"
            dut.io.in.zipWithIndex.foreach { case (lane, index) =>
              lane.valid.poke(((requestMask & (1 << index)) != 0).B)
              pokePayload(lane.bits, data(index))
            }
            dut.io.muxSel.poke(selectMask.U)
            dut.io.arbiterOut.ready.poke(ready.B)
            expectPayload(dut.io.typedMuxOut, muxBits, clue)
            expectPayload(dut.io.rawMuxOut, muxBits, clue)
            expectPayload(dut.io.arbiterOut.bits, data(chosen), clue)
            dut.io.arbiterOut.valid.expect((requestMask != 0).B, clue)
            dut.io.chosen.expect(chosen.U, clue)
            dut.io.ready.expect((if (ready) (1 << (chosen + 1)) - 1 else 0).U, clue)
          }
        }
      }
    }

    it("supports single-input bypasses and SV cores in either elaboration order") {
      val cases = Seq(SvArbiterCase(17, 1), SvArbiterCase(51, 3, "vecBundle"))
      for (order <- Seq(cases, cases.reverse); prefix <- Seq("", "soc_")) withPrefix(prefix) {
        val rtl = ChiselStage.emitSystemVerilog(new SvArbiterBank(order))
        val modules = """(?m)^\s*module\s+(\w+)\b""".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
        assert(modules.count(_ == s"${prefix}SvArbiter") == 1, modules.mkString(", "))
        assert(!modules.exists(_.startsWith("SvArbiterInline")), modules.mkString(", "))
      }
    }

    it("bypasses single-input Bundle and Vec[Bundle] in wrapper and factory for every handshake combination") {
      for (c <- Seq(SvArbiterCase(1, 1), SvArbiterCase(17, 1), SvArbiterCase(51, 1, "vecBundle"));
        factory <- Seq(false, true)) {
        exerciseBypass(c, factory)
      }
    }

    it("elaborates no factory child or wrapper ExtModule and registers no SV for single input") {
      for (c <- Seq(SvArbiterCase(17, 1), SvArbiterCase(51, 1, "vecBundle"));
        factory <- Seq(false, true); prefix <- Seq("", "soc_")) withPrefix(prefix) {
        val elaborated = ChiselStage.elaborate(new SvArbiterOneRtlTop(c, factory))
        val clue = s"case=$c factory=$factory prefix=$prefix"
        assert(!elaborated.annotations.exists(_.isInstanceOf[BlackBoxInlineAnno]), clue)
        val fir = elaborated.serialize
        assert("""(?m)^\s*extmodule\b""".r.findFirstIn(fir).isEmpty, clue)
        val childCount = """(?m)^\s*inst\s+\w+\s+of\s+""".r.findAllIn(fir).size
        assert(childCount == (if (factory) 0 else 1), s"$clue childCount=$childCount")
        val moduleCount = """(?m)^\s*(?:public\s+)?module\s+""".r.findAllIn(fir).size
        assert(moduleCount == (if (factory) 1 else 2), s"$clue moduleCount=$moduleCount")
        val rtl = ChiselStage.emitSystemVerilog(new SvArbiterOneRtlTop(c, factory))
        val modules = """(?m)^\s*module\s+(\w+)\b""".r.findAllMatchIn(rtl).map(_.group(1)).toSeq
        assert(modules == Seq("SvArbiterOneRtlTop"), s"$clue modules=$modules")
        assert(!"""(?m)^\s*(?:soc_)?SvArbiter(?:_[0-9]+)?\s+(?:#\s*\(|\w+\s*\()""".r
          .findFirstIn(rtl).isDefined, clue)
        assert(!rtl.contains("[-1:0]"), clue)
      }
    }

    it("infers unknown Bundle and Vec[Bundle] widths across both single-input bypass forms") {
      for (c <- Seq(SvArbiterCase(17, 1), SvArbiterCase(51, 1, "vecBundle")); factory <- Seq(false, true)) {
        exerciseBypass(c, factory, inferWidths = true)
      }
    }

    it("keeps explicit raw INPUT_SIZE=1 supported with a one-bit chosen boundary") {
      simulateRaw(new SvArbiterRawOneTop) { dut =>
        for (valid <- Seq(false, true); ready <- Seq(false, true); bits <- Seq(BigInt(0), BigInt("1a5c3", 16))) {
          pokePayload(dut.in.bits, bits)
          dut.in.valid.poke(valid.B)
          dut.out.ready.poke(ready.B)
          dut.out.valid.expect(valid.B)
          dut.in.ready.expect(ready.B)
          dut.chosen.expect(0.U)
          expectPayload(dut.out.bits, bits)
        }
      }
      val rtl = ChiselStage.emitSystemVerilog(new SvArbiterRawOneTop)
      assert(rtl.replaceAll("\\s+", " ").contains(
        "output wire [((INPUT_SIZE > 1) ? $clog2(INPUT_SIZE) : 1) - 1:0] io_chosen"))
      assert(!rtl.contains("[-1:0]"))
    }

    it("rejects nonpositive lane counts and invalid widths only at SV boundaries") {
      for ((width, size) <- Seq((0, 1), (-1, 1), (8, 0), (8, -1))) {
        intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val bad = Module(new SvArbiterImpl(width, size))
          })
        }
      }
      for (size <- Seq(0, -1); factory <- Seq(false, true)) {
        val error = intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val gen = new WrapperTestPacket(17)
            val bad = if (factory) SvArbiter(gen, size) else Module(new SvArbiter(gen, size)).io
          })
        }
        assert(error.getMessage.contains("at least one input"))
      }
      for (unknown <- Seq(false, true); factory <- Seq(false, true)) {
        val error = intercept[IllegalArgumentException] {
          ChiselStage.emitCHIRRTL(new RawModule {
            val gen: Data = if (unknown) new SvArbiterInferredPayload else new Bundle { val empty = UInt(0.W) }
            val bad = if (factory) SvArbiter(gen, 2) else Module(new SvArbiter(gen, 2)).io
          })
        }
        assert(error.getMessage.contains("known, positive data width"))
      }
    }
  }
}
