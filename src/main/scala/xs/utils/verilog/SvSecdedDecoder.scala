package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}
import xs.utils.Decoding

/** Width calculation shared by the SECDED wrappers; ECC_BITS includes overall parity. */
object SvSecded {
  /** Smallest r with 2^r >= DATA_BITS + r + 1, plus one overall parity bit. */
  def eccBits(dataBits: Int): Int = {
    require(dataBits > 0, "SvSecded DATA_BITS must be positive")
    val hammingBits = (1 to 32).find(r => (1L << r) >= dataBits.toLong + r + 1).get
    val bits = hammingBits + 1
    require(dataBits.toLong + bits <= Int.MaxValue, "SvSecded codeword width exceeds the positive Int range")
    bits
  }
}

/** SECDED result with short error flags and the shared Decoding interface. */
abstract class SvSecdedDecoding extends Decoding {
  def ce: Bool
  def ue: Bool
  final def correctable: Bool = ce
  final def uncorrectable: Bool = ue
}

/** Combinational SECDED decoder; detects double errors without attempting correction. */
class SvSecdedDecoderImpl(dataBits: Int = 64)
    extends VlogExtModule(Map("DATA_BITS" -> dataBits, "ECC_BITS" -> SvSecded.eccBits(dataBits))) {
  val eccBits: Int = SvSecded.eccBits(dataBits)
  val io = IO(new Bundle {
    val in = Input(new SvSecdedEncoded(dataBits))
    val out = Output(UInt(dataBits.W))
    val ce = Output(Bool())
    val ue = Output(Bool())
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvSecdedDecoderImpl)
  rtl.register(this, SvSecdedDecoderImpl)
}

object SvSecdedDecoderImpl extends VlogModule {
  override val moduleName = "SvSecdedDecoder"
  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer DATA_BITS = 64,
       |  parameter integer ECC_BITS = ${SvSecded.eccBits(64)}
       |) (
       |  input  wire  [DATA_BITS - 1:0] io_in_data,
       |  input  wire  [ECC_BITS - 1:0]  io_in_ecc,
       |  output logic [DATA_BITS - 1:0] io_out,
       |  output wire                    io_ce,
       |  output wire                    io_ue
       |);
       |  // ECC_BITS is computed in Scala, includes overall parity, and must be
       |  // minimal for DATA_BITS >= 1. io_in_ecc layout: {overall, hamming parity}.
       |  // Guarantees apply only to zero, one or two errors per complete codeword.
       |  // Parity-only single errors assert ce without changing data.
       |  // Double errors assert ue and pass the received data through.
       |  localparam integer HAMMING_BITS = ECC_BITS - 1;
       |  localparam integer HAMMING_SIZE = DATA_BITS + HAMMING_BITS;
       |  logic [HAMMING_SIZE - 1:0] hamming_word;
       |  logic [HAMMING_SIZE - 1:0] syndrome_terms [HAMMING_BITS];
       |  logic [HAMMING_BITS - 1:0] syndrome;
       |  wire overall_error;
       |
       |  // Chisel equivalent: hamming_word := VecInit((1 to HAMMING_SIZE).map(p =>
       |  //   if (isPow2(p)) io_in_ecc(log2Ceil(p))
       |  //   else io_in_data(p - 1 - log2Ceil(p)))).asUInt
       |  always_comb begin
       |    for (integer p = 1; p <= HAMMING_SIZE; p = p + 1) begin
       |      if ((p & (p - 1)) == 0) begin
       |        hamming_word[p - 1] = io_in_ecc[$$clog2(p)];
       |      end else begin
       |        hamming_word[p - 1] = io_in_data[p - 1 - $$clog2(p)];
       |      end
       |    end
       |  end
       |
       |  // Chisel equivalent (each j): syndrome(j) := VecInit((1 to HAMMING_SIZE).map(p =>
       |  //   hamming_word(p - 1) && (((p >> j) & 1) != 0).B)).asUInt.xorR
       |  always_comb begin
       |    for (integer j = 0; j < HAMMING_BITS; j = j + 1) begin
       |      for (integer p = 1; p <= HAMMING_SIZE; p = p + 1) begin
       |        syndrome_terms[j][p - 1] = hamming_word[p - 1] & (((p >> j) & 1) != 0);
       |      end
       |      syndrome[j] = ^syndrome_terms[j];
       |    end
       |  end
       |  assign overall_error = ^{io_in_ecc, io_in_data};
       |  assign io_ce = overall_error;
       |  assign io_ue = (|syndrome) & ~overall_error;
       |
       |  // Insert each parity position into a zero-based data index. All calls
       |  // use elaboration constants; the stage array is not runtime state.
       |  // Chisel equivalent: (0 until HAMMING_BITS).scanLeft(bit_index + 1) {
       |  //   (p, j) => p + (if (p >= (1 << j)) 1 else 0) }.last
       |  function automatic integer data_position(input integer bit_index);
       |    integer position_stage [HAMMING_BITS + 1];
       |    begin
       |      position_stage[0] = bit_index + 1;
       |      for (integer j = 0; j < HAMMING_BITS; j = j + 1) begin
       |        position_stage[j + 1] = position_stage[j] + ((position_stage[j] >= (1 << j)) ? 1 : 0);
       |      end
       |      data_position = position_stage[HAMMING_BITS];
       |    end
       |  endfunction
       |
       |  // Chisel equivalent: io_out := io_in_data ^ VecInit(
       |  //   (1 to HAMMING_SIZE).filterNot(isPow2).map(p =>
       |  //     overall_error && (syndrome === p.U))).asUInt
       |  always_comb begin
       |    for (integer i = 0; i < DATA_BITS; i = i + 1) begin
       |      io_out[i] = io_in_data[i] ^
       |        (overall_error & (syndrome == HAMMING_BITS'(data_position(i))));
       |    end
       |  end
       |endmodule
       |""".stripMargin
}

/** No clock/reset or pipeline latency; io.out is valid only when !io.ue. */
class SvSecdedDecoder(dataBits: Int = 64) extends RawModule {
  val eccBits: Int = SvSecded.eccBits(dataBits)
  val io = IO(new Bundle {
    val in = Input(new SvSecdedEncoded(dataBits))
    val out = Output(UInt(dataBits.W))
    val ce = Output(Bool())
    val ue = Output(Bool())
  })
  private val core = Module(new SvSecdedDecoderImpl(dataBits))
  core.io.in := io.in
  io.out := core.io.out
  io.ce := core.io.ce
  io.ue := core.io.ue
  inlineInstance(this)
}

object SvSecdedDecoder {
  /** Infer DATA_BITS from data and require the matching ECC width. */
  def apply(data: UInt, ecc: UInt)(implicit sourceInfo: SourceInfo): SvSecdedDecoding = {
    requireIsHardware(data, "SvSecdedDecoder data")
    requireIsHardware(ecc, "SvSecdedDecoder ECC")
    require(data.isWidthKnown, "SvSecdedDecoder requires a known data width")
    val dataBits = data.getWidth
    val eccBits = SvSecded.eccBits(dataBits)
    require(ecc.isWidthKnown && ecc.getWidth == eccBits,
      "SvSecdedDecoder ECC width must equal SvSecded.eccBits(DATA_BITS)")
    val decoder = Module(new SvSecdedDecoder(dataBits))
    decoder.io.in.data := data
    decoder.io.in.ecc := ecc
    new SvSecdedDecoding {
      val uncorrected = data
      val corrected = decoder.io.out
      val ce = decoder.io.ce
      val ue = decoder.io.ue
    }
  }
}
