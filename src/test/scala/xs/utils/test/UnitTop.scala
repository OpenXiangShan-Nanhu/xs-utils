package xs.utils.test

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.log2Ceil
import circt.stage.ChiselStage
import firrtl.AnnotationSeq
import xs.utils.arb.VipArbiter

object VipArbTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new ChiselStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new VipArbiter(UInt(16.W), 5)))
}