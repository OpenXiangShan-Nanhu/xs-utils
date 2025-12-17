package xs.utils.test

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.log2Ceil
import firrtl.AnnotationSeq
import xs.utils.arb.VipArbiter
import xs.utils.stage.XsStage
import xs.utils.verilog._

class PriorityEncoderHighMod(width:Int) extends Module {
  val io = IO(new Bundle {
    val i_data = Input(UInt(width.W))
    val o_code = Output(UInt(log2Ceil(width).W))
  })
  io.o_code := PriorityEncoderHigh(io.i_data)
}

object PriorityEncoderHighTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new XsStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new PriorityEncoderHighMod(16)))
}

class PriorityEncoderLowMod(width:Int) extends Module {
  val io = IO(new Bundle {
    val i_data = Input(UInt(width.W))
    val o_code = Output(UInt(log2Ceil(width).W))
  })
  io.o_code := PriorityEncoderLow(io.i_data)
}

object PriorityEncoderLowTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new XsStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new PriorityEncoderLowMod(24)))
}

class PriorityEncoderOneHotHighMod(width:Int) extends Module {
  val io = IO(new Bundle {
    val i_data = Input(UInt(width.W))
    val o_code = Output(UInt(width.W))
  })
  io.o_code := PriorityEncoderOneHotHigh(io.i_data)
}

object PriorityEncoderOneHotHighTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new XsStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new PriorityEncoderOneHotHighMod(35)))
}

class PriorityEncoderOneHotLowMod(width:Int) extends Module {
  val io = IO(new Bundle {
    val i_data = Input(UInt(width.W))
    val o_code = Output(UInt(width.W))
  })
  io.o_code := PriorityEncoderOneHotLow(io.i_data)
}

object PriorityEncoderOneHotLowTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new XsStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new PriorityEncoderOneHotLowMod(18)))
}

object VipArbTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new XsStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new VipArbiter(UInt(16.W), 5)))
}