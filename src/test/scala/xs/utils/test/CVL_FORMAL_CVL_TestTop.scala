package xs.utils.test

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.ChiselStage
import firrtl.AnnotationSeq
import xs.utils.cvl.examples.design._

object CVL_FORMAL_CVL_TestTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new ChiselStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new FIFO_CTRL(4, 8)(useCVL = true, useExternSVA = false, useAIP = false)))
}
