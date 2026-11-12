package xs.utils.test.chiselsim

import chisel3._
import chisel3.experimental.IntParam
import chisel3.util.{ArbiterIO, Decoupled, DecoupledIO}
import circt.stage.ChiselStage
import org.scalatest.funspec.AnyFunSpec
import xs.utils.verilog._

class SvDefaultsSpec extends AnyFunSpec {
  describe("SV parameter defaults") {
    it("keeps no-argument ExtModule parameters consistent with their SV declarations") {
      ChiselStage.emitCHIRRTL(new Module {
        val cores: Seq[(VlogExtModule, VlogModule)] = Seq(
          Module(new SvMux1HImpl()) -> SvMux1HImpl,
          Module(new SvPriorityMuxImpl()) -> SvPriorityMuxImpl,
          Module(new SvPriorityEncoderImpl()) -> SvPriorityEncoderImpl,
          Module(new SvPriorityEncoder1HImpl()) -> SvPriorityEncoder1HImpl,
          Module(new SvPrefixReduceImpl()()) -> SvPrefixReduceImpl(),
          Module(new SvPrefixReduceImpl(op = SvPrefixReduce.And)()) -> SvPrefixReduceImpl(SvPrefixReduce.And),
          Module(new SvPrefixReduceImpl(op = SvPrefixReduce.Xor)()) -> SvPrefixReduceImpl(SvPrefixReduce.Xor),
          Module(new SvOHToUIntImpl()) -> SvOHToUIntImpl,
          Module(new SvOHToMaskImpl()) -> SvOHToMaskImpl,
          Module(new SvMaskToOHImpl()) -> SvMaskToOHImpl,
          Module(new SvTreePLRUImpl()) -> SvTreePLRUImpl,
          Module(new SvUIntToOHImpl()) -> SvUIntToOHImpl,
          Module(new SvPopcountImpl()) -> SvPopcountImpl,
          Module(new SvSecdedEncoderImpl()) -> SvSecdedEncoderImpl,
          Module(new SvSecdedDecoderImpl()) -> SvSecdedDecoderImpl,
          Module(new SvArbiterImpl()) -> SvArbiterImpl,
          Module(new SvLivelockFreeArbiterImpl()) -> SvLivelockFreeArbiterImpl,
          Module(new SvFastArbiterImpl()) -> SvFastArbiterImpl,
          Module(new SvFastQueueD1Impl()) -> SvFastQueueD1Impl,
          Module(new SvFastQueueD2Impl()) -> SvFastQueueD2Impl,
          Module(new SvFastQueueD3Impl()) -> SvFastQueueD3Impl,
          Module(new SvFastQueueDNImpl()) -> SvFastQueueDNImpl,
          Module(new SvMimoQueueImpl()) -> SvMimoQueueImpl
        )
        cores.foreach { case (core, template) =>
          val source = new VlogRegistry().sources(template).last.text
          val defaults = """parameter\s+(?:integer\s+)?(\w+)\s*=\s*(\d+)""".r
            .findAllMatchIn(source).map(m => m.group(1) -> BigInt(m.group(2))).toMap
          core.params.foreach { case (name, value) =>
            assert(value == IntParam(defaults(name)), s"${template.moduleName}.$name")
          }
        }
      })
    }

    it("checks wrapper defaults, explicit arbiter dimensions and named parameter overrides") {
      ChiselStage.emitCHIRRTL(new Module {
        override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
        val mux = Module(new SvMux1H())
        val priority = Module(new SvPriorityMux())
        val arb = Module(new SvArbiter(UInt(32.W), 2))
        val rr = Module(new SvRRArbiter(UInt(32.W), 2))
        val vip = Module(new SvVipArbiter(UInt(32.W), 2))
        val unified = Module(new SvLivelockFreeArbiter(UInt(32.W), 2))
        val queue = Module(new SvFastQueue())
        val mimo = Module(new SvMimoQueue())
        val payloads: Seq[UInt] = Seq(mux.io.out, priority.io.out, arb.io.out.bits,
          rr.io.out.bits, vip.io.out.bits, unified.io.out.bits, queue.io.deq.bits, mimo.io.deq.head.bits)
        assert(payloads.forall(_.getWidth == 32))
        assert(mux.io.in.size == 2 && priority.io.in.size == 2)
        assert(arb.io.in.size == 2 && rr.io.in.size == 2 && vip.io.in.size == 2)
        assert(queue.io.waterline.getWidth == 3)
        assert(mimo.io.enq.size == 1 && mimo.io.deq.size == 1 && mimo.io.count.getWidth == 3)
        val secdedEncoder = Module(new SvSecdedEncoder())
        val secdedDecoder = Module(new SvSecdedDecoder())
        assert(secdedEncoder.eccBits == 8 && secdedEncoder.io.in.getWidth == 64 && secdedEncoder.io.out.data.getWidth == 64 && secdedEncoder.io.out.ecc.getWidth == 8)
        assert(secdedDecoder.eccBits == 8 && secdedDecoder.io.in.data.getWidth == 64 && secdedDecoder.io.in.ecc.getWidth == 8 && secdedDecoder.io.out.getWidth == 64)
        val prefixReduce = Module(new SvPrefixReduce())
        assert(prefixReduce.io.in.getWidth == 32 && prefixReduce.io.out.getWidth == 32)
        val popcount = Module(new SvPopcount())
        assert(popcount.io.in.getWidth == 2 && popcount.io.out.getWidth == 2)
        val decoder = Module(new SvUIntToOH())
        assert(decoder.io.in.getWidth == 1 && decoder.io.out.getWidth == 2)
        val mask = Module(new SvOHToMask())
        assert(mask.io.in.getWidth == 8 && mask.io.out.getWidth == 8)
        val treePLRU = Module(new SvTreePLRU())
        assert(treePLRU.io.touch.bits.getWidth == 4 && treePLRU.io.out.getWidth == 4)

        val wide = Module(new SvMux1H(gen = UInt(65.W)))
        val three = Module(new SvArbiter(UInt(32.W), n = 3))
        val rrThree = Module(new SvRRArbiter(UInt(32.W), n = 3))
        val vipThree = Module(new SvVipArbiter(UInt(32.W), size = 3))
        val fast = Module(new SvFastArbiter(UInt(32.W), 2))
        val latched = Module(new SvLatchFastArbiter(UInt(32.W), 2))
        assert(Seq(fast, latched).forall(_.io.out.bits.getWidth == 32))
        val queueFive = Module(new SvFastQueue(size = 5))
        val mimoTwo = Module(new SvMimoQueue(enqNum = 2, deqNum = 2))
        val dnFive = Module(new SvFastQueueDNImpl(depth = 5))
        assert(wide.io.out.getWidth == 65 && wide.io.in.size == 2)
        assert(three.io.in.size == 3 && three.io.out.bits.getWidth == 32)
        assert(rrThree.io.in.size == 3 && rrThree.io.out.bits.getWidth == 32)
        assert(vipThree.io.in.size == 3 && vipThree.io.out.bits.getWidth == 32)
        assert(queueFive.io.waterline.getWidth == 6 && queueFive.io.deq.bits.getWidth == 32)
        assert(mimoTwo.io.enq.size == 2 && mimoTwo.io.deq.size == 2)
        assert(dnFive.io.waterline.getWidth == 6 && dnFive.io.deq.bits.getWidth == 32)
      })
    }

    it("supports omitted factory configuration while preserving payload type inference") {
      ChiselStage.emitCHIRRTL(new Module {
        override def resetType: Module.ResetType.Type = Module.ResetType.Asynchronous
        val arb: ArbiterIO[UInt] = SvArbiter()
        val rr: ArbiterIO[UInt] = SvRRArbiter()
        val vip: ArbiterIO[UInt] = SvVipArbiter()
        val fastWrapper: ArbiterIO[UInt] = SvFastArbiter()
        val fastAlias: ArbiterIO[UInt] = xs.utils.FastArbiter()
        assert(Seq(fastWrapper, fastAlias).forall(io => io.in.size == 2 && io.out.bits.getWidth == 32))
        val unified: ArbiterIO[UInt] = SvLivelockFreeArbiter()
        assert(unified.in.size == 2 && unified.out.bits.getWidth == 32)
        val latched: ArbiterIO[UInt] = SvLatchFastArbiter()
        assert(latched.in.size == 2 && latched.out.bits.getWidth == 32)
        val vipThree: ArbiterIO[UInt] = SvVipArbiter(size = 3)
        val arbThree: ArbiterIO[UInt] = SvArbiter(n = 3)
        val rrThree: ArbiterIO[UInt] = SvRRArbiter(n = 3)
        val signed: ArbiterIO[SInt] = SvArbiter(SInt(17.W))
        val signedRR: ArbiterIO[SInt] = SvRRArbiter(SInt(17.W))
        assert(arb.in.size == 2 && arb.out.bits.getWidth == 32)
        assert(rr.in.size == 2 && rr.out.bits.getWidth == 32)
        assert(vip.in.size == 2 && vip.out.bits.getWidth == 32)
        assert(vipThree.in.size == 3 && vipThree.out.bits.getWidth == 32)
        assert(arbThree.in.size == 3 && rrThree.in.size == 3)
        assert(signed.out.bits.getWidth == 17 && signedRR.out.bits.getWidth == 17)
        def input = Wire(Decoupled(UInt(17.W)))
        val queues: Seq[DecoupledIO[UInt]] = Seq(
          SvFastQueueRaw(input), SvFastQueueRaw(input, noX = true),
          SvFastQueueRaw(input, name = Some("namedQueue")),
          SvFastQueue(input), SvFastQueueNoX(input), new SvFastQueueFactory()(input)
        )
        assert(queues.forall(_.bits.getWidth == 17))
      })
    }
  }
}
