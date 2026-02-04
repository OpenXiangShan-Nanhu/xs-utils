package xs.utils.test

import chisel3._
import chisel3.experimental.{SourceInfo, noPrefix}
import chisel3.experimental.hierarchy.core._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.Decoupled
import circt.stage.ChiselStage
import firrtl.AnnotationSeq
import org.chipsalliance.cde.config.{Config, Parameters}
import xs.utils.FileRegisters
import xs.utils.arb.VipArbiter
import xs.utils.debug.{HAssert, HardwareAssertionKey, HwaParams}

object ModAHelper {
  def xor(in0:Bool, in1:Bool)(implicit p: Parameters, s: SourceInfo):Unit = {
    HAssert(in0 ^ in1, cf"assert xor ${in0}%x ^ ${in1}%x")(p, s)
  }
}

class ModA(implicit p:Parameters) extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val c = Input(Bool())
    val d = Input(Bool())
    val z = Output(Bool())
  })
  HAssert(io.a, cf"assert A ${io.a}%x")
  HAssert(io.b, cf"assert B ${io.a}%x")
  ModAHelper.xor(io.a, io.b)
  ModAHelper.xor(io.c, io.d)
  when(io.a) {
    ModAHelper.xor(io.b, io.c)
  }.otherwise {
    ModAHelper.xor(io.b, io.d)
  }
  io.z := io.a & io.b ^ io.c & io.d
}

class ModBIO extends Bundle {
  val ain = Vec(2, Flipped(Decoupled(UInt(64.W))))
  val aout = Decoupled(UInt(64.W))
}

@instantiable
class ModB(implicit p:Parameters) extends Module {
  @public val io = IO(new ModBIO)
  io.aout <> VipArbiter(io.ain)

  when(io.aout.fire) {
    HAssert(io.ain(0).fire || io.ain(1).fire)
  }
  when(io.aout.fire) {
    HAssert(io.aout.bits === io.ain(0).bits || io.aout.bits === io.ain(1).bits)
  }
  HAssert.placePipe(1, name = "ModBPipe")
  @public val hwa = HAssert.exportIO
}

class HAssertTest extends Module {
  override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
  private val size = 2
  implicit val config:Parameters = new Config((_, _, _) => {
    case HardwareAssertionKey => HwaParams(enable = true)
  })
  val io = IO(new Bundle{
    val a = Input(Vec(size, Bool()))
    val b = Input(Vec(size, Bool()))
    val c = Input(Vec(size, Bool()))
    val d = Input(Vec(size, Bool()))
    val z = Output(Vec(size, Bool()))
    val arb = Vec(size, new ModBIO)
    val hwa = Option.when(config(HardwareAssertionKey).enable)(Decoupled(UInt(config(HardwareAssertionKey).maxInfoBits.W)))
  })

  private val modSeq0 = Seq.fill(size / 2)(Module(new ModA))

  private val mbDef = Definition(new ModB)

  private val mbs = Seq.tabulate(size)(i => noPrefix {
    val inst = Instance(mbDef)
    inst.suggestName(s"mb_$i")
    inst.io <> io.arb(i)
    HAssert.fromIO(inst.hwa)
    inst
  })

  private val modSeq1 = Seq.fill(size / 2)(Module(new ModA))
  private val modSeq = (modSeq0 ++ modSeq1)
  for(i <- modSeq.indices) {
    modSeq(i).io.a := io.a(i)
    modSeq(i).io.b := io.b(i)
    modSeq(i).io.c := io.c(i)
    modSeq(i).io.d := io.d(i)
    io.z(i) := modSeq(i).io.z
  }
  HAssert.placePipe(1, name = "ModA")

  private val top = HAssert.placePipe(2, moduleTop = true, name = "HAssertTop")
  HAssert.release(top, "hwa", "test")

  io.hwa.foreach(hwa => hwa <> top.get.head.bus.get)
}

object HAssertTestTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new ChiselStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new HAssertTest))
  FileRegisters.write("build", "HAssertTest")
}