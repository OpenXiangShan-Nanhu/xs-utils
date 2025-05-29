package xs.utils.test

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import firrtl.AnnotationSeq
import org.chipsalliance.cde.config.{Config, Parameters}
import xs.utils.FileRegisters
import xs.utils.debug.{HAssert, HardwareAssertionKey, HwaParams}
import xs.utils.stage.XsStage
import xs.utils.cvl.advanced._

class Mod extends Module {
  val io = IO(new Bundle {
    val req = Input(Vec(3, Valid(UInt(32.W))))
    val resp = Input(Vec(3, Valid(UInt(32.W))))
  })

  // Use the advanced CVL assertions
  CVL_ASSERT_REQ_ACK(
    true,
    true,
    false,
    clock,
    reset,
    "sva_assert_req_ack",
    min_time = 1,
    max_time = 3,
    reqWidth = 3,
    io.req,
    ackWidth = 3,
    io.resp
  )
}

object CVL_ASSERT_REQ_ACK_TestTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new XsStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new Mod))
}