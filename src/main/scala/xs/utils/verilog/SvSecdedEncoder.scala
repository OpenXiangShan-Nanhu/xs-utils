package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}

/** Data and ECC fields of a systematic SECDED codeword. */
class SvSecdedEncoded(dataBits: Int) extends Bundle {
  val data = UInt(dataBits.W)
  val ecc = UInt(SvSecded.eccBits(dataBits).W)
}

/** Combinational even-parity extended Hamming encoder, in systematic form. */
class SvSecdedEncoderImpl(dataBits: Int = 64)
    extends VlogExtModule(Map("DATA_BITS" -> dataBits, "ECC_BITS" -> SvSecded.eccBits(dataBits))) {
  val eccBits: Int = SvSecded.eccBits(dataBits)
  val io = IO(new Bundle {
    val in = Input(UInt(dataBits.W))
    val out = Output(new SvSecdedEncoded(dataBits))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvSecdedEncoderImpl)
  rtl.register(this, SvSecdedEncoderImpl)
}

object SvSecdedEncoderImpl extends VlogModule {
  override val moduleName = "SvSecdedEncoder"
  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer DATA_BITS = 64,
       |  parameter integer ECC_BITS = ${SvSecded.eccBits(64)}
       |) (
       |  input  wire  [DATA_BITS - 1:0] io_in,
       |  output wire  [DATA_BITS - 1:0] io_out_data,
       |  output wire  [ECC_BITS - 1:0]  io_out_ecc
       |);
       |  // ECC_BITS is supplied by Scala, includes overall parity, and must be
       |  // minimal for DATA_BITS >= 1. io_out_ecc layout: {overall, hamming parity}.
       |  localparam integer HAMMING_BITS = ECC_BITS - 1;
       |  localparam integer HAMMING_SIZE = DATA_BITS + HAMMING_BITS;
       |  logic [HAMMING_SIZE - 1:0] data_hamming;
       |  logic [HAMMING_SIZE - 1:0] parity_terms [HAMMING_BITS];
       |  logic [HAMMING_BITS - 1:0] parity;
       |
       |  // Chisel equivalent: data_hamming := VecInit((1 to HAMMING_SIZE).map(p =>
       |  //   if (isPow2(p)) false.B else io_in(p - 1 - log2Ceil(p)))).asUInt
       |  always_comb begin
       |    for (integer p = 1; p <= HAMMING_SIZE; p = p + 1) begin
       |      if ((p & (p - 1)) == 0) begin
       |        data_hamming[p - 1] = 1'b0;
       |      end else begin
       |        data_hamming[p - 1] = io_in[p - 1 - $$clog2(p)];
       |      end
       |    end
       |  end
       |
       |  // Chisel equivalent (each j): parity(j) := VecInit((1 to HAMMING_SIZE).map(p =>
       |  //   data_hamming(p - 1) && (((p >> j) & 1) != 0).B)).asUInt.xorR
       |  always_comb begin
       |    for (integer j = 0; j < HAMMING_BITS; j = j + 1) begin
       |      for (integer p = 1; p <= HAMMING_SIZE; p = p + 1) begin
       |        parity_terms[j][p - 1] = data_hamming[p - 1] & (((p >> j) & 1) != 0);
       |      end
       |      parity[j] = ^parity_terms[j];
       |    end
       |  end
       |  assign io_out_data = io_in;
       |  assign io_out_ecc = {^{parity, io_in}, parity};
       |endmodule
       |""".stripMargin
}

/** Inline wrapper; ECC width is derived in Scala, never independently configurable. */
class SvSecdedEncoder(dataBits: Int = 64) extends RawModule {
  val eccBits: Int = SvSecded.eccBits(dataBits)
  val io = IO(new Bundle {
    val in = Input(UInt(dataBits.W))
    val out = Output(new SvSecdedEncoded(dataBits))
  })
  private val core = Module(new SvSecdedEncoderImpl(dataBits))
  core.io.in := io.in
  io.out := core.io.out
  inlineInstance(this)
}

object SvSecdedEncoder {
  def apply(in: UInt)(implicit sourceInfo: SourceInfo): SvSecdedEncoded = {
    requireIsHardware(in, "SvSecdedEncoder input")
    require(in.isWidthKnown, "SvSecdedEncoder requires a known data width")
    val encoder = Module(new SvSecdedEncoder(in.getWidth))
    encoder.io.in := in
    encoder.io.out
  }
}
