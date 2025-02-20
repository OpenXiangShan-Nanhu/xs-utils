
package xs.utils.common

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
}