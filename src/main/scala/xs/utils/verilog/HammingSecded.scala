package xs.utils.verilog

import chisel3._
import chisel3.util._
import xs.utils.GlobalData

/**
 * Parameterized Hamming SECDED (Single Error Correction, Double Error Detection).
 *
 * Architecture:
 *   - Pure SystemVerilog kernel (parameterized, self-contained)
 *   - Thin Chisel ExtModule wrapper for instantiation
 *   - Convenience API via companion object
 *
 * The SV code uses parameter + function + for-loops to implement the Hamming
 * algorithm. No Scala logic generates SV code — the SV is a string constant
 * that can be directly copied to a pure-SV project in the future.
 */
object HammingSecded {
  def calcEccBits(dataBits: Int): Int = {
    var r = 0
    while ((1 << r) < dataBits + r + 1) r += 1
    r + 1
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

class HammingSecdedEncoder(dataBits: Int, eccBits:Int) extends ExtModule(Map(
  "DATA_BITS" -> dataBits,
  "ECC_BITS" -> eccBits
)) {
  val io = FlatIO(new Bundle {
    val i_data = Input(UInt(dataBits.W))
    val o_ecc = Output(UInt(eccBits.W))
  })
  private val modName = s"${GlobalData.prefix}HammingSecdedEncoder"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""// Hamming SECDED Encoder — parameterized, self-contained SystemVerilog
       |// Can be used standalone in a pure-SV project (just rename the module).
       |//
       |// Hamming SECDED overview:
       |//   - Data bits are placed at non-power-of-2 positions in a codeword
       |//   - Parity bit k covers all codeword positions whose bit k is set
       |//   - An extra overall-parity bit enables double-error detection
       |//
       |module $modName #(
       |  parameter int DATA_BITS = 64,
       |  parameter int ECC_BITS  = 7
       |)(
       |  input  logic [DATA_BITS-1:0]      i_data,
       |  output logic [ECC_BITS-1:0]       o_ecc   // {overall_parity, syndrome}
       |);
       |
       |  // ---------------------------------------------------------------
       |  // Step 1: Calculate how many parity bits we need.
       |  //         We need r such that 2^r >= DATA_BITS + r + 1.
       |  //         Total ECC bits = parity bits + 1 (overall parity) = ECC_BITS
       |  // ---------------------------------------------------------------
       |  function automatic int calc_parity_bits(int d);
       |    int r;
       |    r = 0;
       |    while ((1 << r) < d + r + 1) r = r + 1;
       |    return r;
       |  endfunction
       |
       |  localparam int PARITY_BITS = calc_parity_bits(DATA_BITS);
       |
       |  // ---------------------------------------------------------------
       |  // Step 2: Map data index to Hamming codeword position.
       |  //         Codeword positions start at 1. Power-of-2 positions
       |  //         (1, 2, 4, 8, ...) are reserved for parity bits.
       |  //         Data bits fill the remaining positions in order.
       |  //
       |  //         Example for DATA_BITS=4:
       |  //           pos: 1  2  3  4  5  6  7
       |  //           use: P1 P2 D0 P4 D1 D2 D3
       |  //           data_pos(0)=3, data_pos(1)=5, data_pos(2)=6, data_pos(3)=7
       |  // ---------------------------------------------------------------
       |  function automatic int data_pos(int idx);
       |    int pos, count;
       |    pos   = 0;
       |    count = -1;
       |    while (count < idx) begin
       |      pos = pos + 1;
       |      // Check if pos is NOT a power of 2 (i.e., it's a data position)
       |      if ((pos & (pos - 1)) != 0)
       |        count = count + 1;
       |    end
       |    return pos;
       |  endfunction
       |
       |  // ---------------------------------------------------------------
       |  // Step 3: Compute parity (syndrome) bits.
       |  //         For each parity bit k (0..PARITY_BITS-1):
       |  //           syndrome[k] = XOR of all data bits whose codeword
       |  //                         position has bit k set.
       |  // ---------------------------------------------------------------
       |  logic [PARITY_BITS-1:0] syndrome;
       |  logic                   overall_parity;
       |
       |  always_comb begin
       |    // Compute each syndrome (parity) bit
       |    for (int k = 0; k < PARITY_BITS; k++) begin
       |      syndrome[k] = 1'b0;
       |      for (int i = 0; i < DATA_BITS; i++) begin
       |        // If bit k of data_pos(i) is 1, this data bit contributes to parity k
       |        if (((data_pos(i) >> k) & 1) == 1)
       |          syndrome[k] = syndrome[k] ^ i_data[i];
       |      end
       |    end
       |
       |    // Overall parity: XOR of all data bits and all syndrome bits
       |    // This enables double-error detection (SECDED)
       |    overall_parity = (^i_data) ^ (^syndrome);
       |
       |    // Output: {overall_parity, syndrome[PARITY_BITS-1:0]}
       |    o_ecc = {overall_parity, syndrome};
       |  end
       |
       |endmodule
       |""".stripMargin)
}

class HammingSecdedDecoder(dataBits: Int, eccBits:Int) extends ExtModule(Map(
  "DATA_BITS" -> dataBits,
  "ECC_BITS" -> eccBits
)) {
  val io = FlatIO(new Bundle {
    val i_data = Input(UInt(dataBits.W))
    val i_ecc = Input(UInt(eccBits.W))
    val o_data = Output(UInt(dataBits.W))
    val o_ce = Output(Bool())
    val o_ue = Output(Bool())
  })
  private val modName = s"${GlobalData.prefix}HammingSecdedDecoder"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""// Hamming SECDED Decoder — parameterized, self-contained SystemVerilog
       |// Can be used standalone in a pure-SV project (just rename the module).
       |//
       |// Decoding process:
       |//   1. Recompute syndrome by XOR-ing stored ECC with recalculated parity
       |//   2. Check overall parity (XOR of all data + all ECC bits)
       |//   3. Classify error:
       |//      - syndrome==0                       → no error
       |//      - syndrome!=0 && overall_parity==1  → single-bit error (correctable)
       |//      - syndrome!=0 && overall_parity==0  → double-bit error (uncorrectable)
       |//   4. For single-bit error: syndrome value = codeword position of the bad bit
       |//      → find which data bit maps to that position and flip it
       |//
       |module $modName #(
       |  parameter int DATA_BITS = 64,
       |  parameter int ECC_BITS  = 7
       |)(
       |  input  logic [DATA_BITS-1:0] i_data,
       |  input  logic [ECC_BITS-1:0]  i_ecc,   // {overall_parity, syndrome}
       |  output logic [DATA_BITS-1:0] o_data,
       |  output logic                 o_ce,    // correctable error (single-bit)
       |  output logic                 o_ue     // uncorrectable error (double-bit)
       |);
       |
       |  // Same helper functions as encoder (duplicated for standalone use)
       |  function automatic int calc_parity_bits(int d);
       |    int r;
       |    r = 0;
       |    while ((1 << r) < d + r + 1) r = r + 1;
       |    return r;
       |  endfunction
       |
       |  localparam int PARITY_BITS = calc_parity_bits(DATA_BITS);
       |
       |  function automatic int data_pos(int idx);
       |    int pos, count;
       |    pos   = 0;
       |    count = -1;
       |    while (count < idx) begin
       |      pos = pos + 1;
       |      if ((pos & (pos - 1)) != 0)
       |        count = count + 1;
       |    end
       |    return pos;
       |  endfunction
       |
       |  // ---------------------------------------------------------------
       |  // Decode logic
       |  // ---------------------------------------------------------------
       |  logic [PARITY_BITS-1:0] syndrome;
       |  logic                   overall_parity;
       |  logic                   syndrome_nonzero;
       |
       |  always_comb begin
       |    // ----- Recompute syndrome -----
       |    // For each parity bit k: XOR the stored ecc[k] with all data bits
       |    // whose codeword position has bit k set.
       |    // If no error occurred, syndrome will be all zeros.
       |    // If a single bit flipped, syndrome = codeword position of the error.
       |    for (int k = 0; k < PARITY_BITS; k++) begin
       |      syndrome[k] = i_ecc[k];  // start with stored parity bit
       |      for (int i = 0; i < DATA_BITS; i++) begin
       |        if (((data_pos(i) >> k) & 1) == 1)
       |          syndrome[k] = syndrome[k] ^ i_data[i];
       |      end
       |    end
       |
       |    // ----- Overall parity check -----
       |    // XOR all data bits and all ECC bits (including overall parity bit)
       |    overall_parity = (^i_data) ^ (^i_ecc);
       |
       |    // ----- Error classification -----
       |    syndrome_nonzero = |syndrome;
       |    o_ce = syndrome_nonzero &  overall_parity;  // odd parity  → 1-bit error
       |    o_ue = syndrome_nonzero & ~overall_parity;  // even parity → 2-bit error
       |
       |    // ----- Correction -----
       |    // Start with original data, then flip the erroneous bit if CE
       |    o_data = i_data;
       |    if (o_ce) begin
       |      for (int i = 0; i < DATA_BITS; i++) begin
       |        // If this data bit's codeword position matches the syndrome,
       |        // it is the erroneous bit — flip it.
       |        if (data_pos(i) == int'(syndrome))
       |          o_data[i] = ~i_data[i];
       |      end
       |    end
       |  end
       |
       |endmodule
       |""".stripMargin)
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
