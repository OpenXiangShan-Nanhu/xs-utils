package xs.utils

import chisel3._
import chisel3.util._

object HammingSecded {
  def calcEccBits(dataBits: Int): Int = {
    var r = 0
    while ((1 << r) < dataBits + r + 1) r += 1
    r + 1
  }

  def dataPos(idx: Int): Int = {
    var pos = 0
    var count = -1
    while (count < idx) {
      pos += 1
      if ((pos & (pos - 1)) != 0) count += 1
    }
    pos
  }

  def encode(data: UInt): UInt = {
    val w = data.getWidth
    val enc = Module(new HammingSecdedEncoder(w, HammingSecded.calcEccBits(w)))
    enc.io.i_data := data
    enc.io.o_ecc
  }

  def decode(data: UInt, ecc: UInt): (UInt, Bool, Bool) = {
    val w = data.getWidth
    val dec = Module(new HammingSecdedDecoder(w, HammingSecded.calcEccBits(w)))
    dec.io.i_data := data
    dec.io.i_ecc := ecc
    (dec.io.o_data, dec.io.o_ce, dec.io.o_ue)
  }
}

class HammingSecdedEncoder(dataBits: Int, eccBits: Int) extends Module {
  val parityBits = eccBits - 1
  val io = IO(new Bundle {
    val i_data = Input(UInt(dataBits.W))
    val o_ecc = Output(UInt(eccBits.W))
  })
  override val desiredName = s"HammingSecdedEncoder_d${dataBits}_e${eccBits}"

  val syndrome = Wire(Vec(parityBits, Bool()))
  for (k <- 0 until parityBits) {
    val contributors = (0 until dataBits).filter(i => ((HammingSecded.dataPos(i) >> k) & 1) == 1)
    syndrome(k) := contributors.map(i => io.i_data(i)).reduce(_ ^ _)
  }

  val syndromeUInt = syndrome.asUInt
  val overallParity = io.i_data.xorR ^ syndromeUInt.xorR
  io.o_ecc := Cat(overallParity, syndromeUInt)
}

class HammingSecdedDecoder(dataBits: Int, eccBits: Int) extends Module {
  val parityBits = eccBits - 1
  val io = IO(new Bundle {
    val i_data = Input(UInt(dataBits.W))
    val i_ecc = Input(UInt(eccBits.W))
    val o_data = Output(UInt(dataBits.W))
    val o_ce = Output(Bool())
    val o_ue = Output(Bool())
  })
  override val desiredName = s"HammingSecdedDecoder_d${dataBits}_e${eccBits}"

  val storedSyndrome = io.i_ecc(parityBits - 1, 0)

  val syndrome = Wire(Vec(parityBits, Bool()))
  for (k <- 0 until parityBits) {
    val contributors = (0 until dataBits).filter(i => ((HammingSecded.dataPos(i) >> k) & 1) == 1)
    syndrome(k) := storedSyndrome(k) ^ contributors.map(i => io.i_data(i)).reduce(_ ^ _)
  }

  val syndromeUInt = syndrome.asUInt
  val overallParity = io.i_data.xorR ^ io.i_ecc.xorR
  val syndromeNonzero = syndromeUInt.orR

  io.o_ce := syndromeNonzero & overallParity
  io.o_ue := syndromeNonzero & ~overallParity

  val corrected = Wire(Vec(dataBits, Bool()))
  for (i <- 0 until dataBits) {
    val pos = HammingSecded.dataPos(i)
    corrected(i) := Mux(io.o_ce && syndromeUInt === pos.U(parityBits.W), ~io.i_data(i), io.i_data(i))
  }
  io.o_data := corrected.asUInt
}

/**
 * Grouped ECC: split data into N groups, each independently SECDED-protected.
 */
class GroupedEcc(val dataBits: Int, val numGroups: Int) {
  require(dataBits % numGroups == 0, s"dataBits($dataBits) must be divisible by numGroups($numGroups)")
  val bitsPerGroup: Int = dataBits / numGroups
  val eccBitsPerGroup: Int = HammingSecded.calcEccBits(bitsPerGroup)
  val totalEccBits: Int = eccBitsPerGroup * numGroups
  val sramWidth: Int = dataBits + totalEccBits

  def encode(data: UInt): UInt = {
    require(data.getWidth == dataBits)
    val groups = data.asTypeOf(Vec(numGroups, UInt(bitsPerGroup.W)))
    VecInit(groups.map(HammingSecded.encode)).asUInt
  }

  def decode(data: UInt, ecc: UInt): (UInt, Vec[Bool], Vec[Bool]) = {
    require(data.getWidth == dataBits)
    require(ecc.getWidth == totalEccBits)
    val dataGroups = data.asTypeOf(Vec(numGroups, UInt(bitsPerGroup.W)))
    val eccGroups = ecc.asTypeOf(Vec(numGroups, UInt(eccBitsPerGroup.W)))
    val results = dataGroups.zip(eccGroups).map { case (d, e) => HammingSecded.decode(d, e) }
    (VecInit(results.map(_._1)).asUInt, VecInit(results.map(_._2)), VecInit(results.map(_._3)))
  }
}
