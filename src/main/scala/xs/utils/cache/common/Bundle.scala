
package xs.utils.cache.common

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import xs.utils.tl._


class PrefetchRecv extends Bundle {
  val addr = UInt(64.W)
  val pf_source = UInt(MemReqSource.reqSourceBits.W)
  val addr_valid = Bool()
  val l2_pf_en = Bool()
}

// custom l2 - l1 CMO inst req
class CMOReq extends Bundle {
  val opcode = UInt(3.W)   // 0-cbo.clean, 1-cbo.flush, 2-cbo.inval, 3-cbo.zero
  val address = UInt(64.W)
}
// custom l2 - l1 CMO inst resp(ack)
class CMOResp extends Bundle {
  val address = UInt(64.W)
  val nderr = Bool()
}

class TPmetaReq(hartIdLen: Int, fullAddressBits: Int, offsetBits: Int) extends Bundle {
  // TODO: rawData's width is determined by L2; when L2's offsetBits change, rawData should change accordingly
  val hartid = UInt(hartIdLen.W)
  val set = UInt(32.W) // determined by TP
  val way = UInt(4.W)
  val wmode = Bool()
  val rawData = Vec(log2Floor(512 / (fullAddressBits - offsetBits)), UInt((fullAddressBits - offsetBits).W))
}

class TPmetaResp(hartIdLen: Int, fullAddressBits: Int, offsetBits: Int) extends Bundle {
  val hartid = UInt(hartIdLen.W)
  val rawData = Vec(log2Floor(512 / (fullAddressBits - offsetBits)), UInt((fullAddressBits - offsetBits).W))
}