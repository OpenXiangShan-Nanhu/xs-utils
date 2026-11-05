package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}
import chisel3.util.log2Ceil

/** Binary index to one-hot decoder; indices outside OUTPUT_SIZE produce zero. */
class SvUIntToOHImpl(outputSize: Int = 2) extends VlogExtModule(Map("OUTPUT_SIZE" -> outputSize)) {
  require(outputSize > 0, "SvUIntToOH OUTPUT_SIZE must be positive")

  val io = IO(new Bundle {
    val in = Input(UInt(math.max(1, log2Ceil(outputSize)).W))
    val out = Output(UInt(outputSize.W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvUIntToOHImpl)
  rtl.register(this, SvUIntToOHImpl)
}

object SvUIntToOHImpl extends VlogModule {
  override val moduleName = "SvUIntToOH"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer OUTPUT_SIZE = 2
       |) (
       |  input  wire  [((OUTPUT_SIZE > 1) ? $$clog2(OUTPUT_SIZE) : 1) - 1:0] io_in,
       |  output logic [OUTPUT_SIZE - 1:0]                                    io_out
       |);
       |  localparam integer INDEX_WIDTH = (OUTPUT_SIZE > 1) ? $$clog2(OUTPUT_SIZE) : 1;
       |
       |  generate
       |    if (OUTPUT_SIZE == 1) begin : gen_singleton
       |      // UIntToOH(in, 1) ignores the index and always returns one.
       |      assign io_out = 1'b1;
       |    end else begin : gen_decode
       |      // Chisel equivalent: io_out := VecInit((0 until OUTPUT_SIZE).map(io_in === _.U)).asUInt
       |      // Unused index codes produce zero for non-power-of-two OUTPUT_SIZE.
       |      always_comb begin
       |        for (integer i = 0; i < OUTPUT_SIZE; i = i + 1) begin : gen_lane
       |          io_out[i] = io_in == INDEX_WIDTH'(i);
       |        end
       |      end
       |    end
       |  endgenerate
       |endmodule
       |""".stripMargin
}

/** Inline wrapper; input width is max(1, log2Ceil(outputSize)). */
class SvUIntToOH(outputSize: Int = 2) extends RawModule {
  require(outputSize > 0, "SvUIntToOH requires a positive output size")

  val io = IO(new Bundle {
    val in = Input(UInt(math.max(1, log2Ceil(outputSize)).W))
    val out = Output(UInt(outputSize.W))
  })

  private val core = Module(new SvUIntToOHImpl(outputSize))
  core.io.in := io.in
  io.out := core.io.out
  inlineInstance(this)
}

/** UIntToOH-compatible factories. The implicit output width is 2^in.getWidth.
  * Explicit width zero returns a zero-width zero; width one always returns one.
  * For larger widths only the low log2Ceil(width) index bits are used, padding
  * shorter inputs with zeros. Unused indices in non-power-of-two outputs return zero.
  */
object SvUIntToOH {
  def apply(in: UInt)(implicit sourceInfo: SourceInfo): UInt = {
    requireIsHardware(in, "SvUIntToOH input")
    require(in.isWidthKnown, "SvUIntToOH requires a known input width or an explicit output width")
    require(in.getWidth < 31, "SvUIntToOH inferred output width exceeds the positive Int range")
    apply(in, 1 << in.getWidth)
  }

  def apply(in: UInt, width: Int)(implicit sourceInfo: SourceInfo): UInt = {
    require(width >= 0, "SvUIntToOH width must be nonnegative")
    requireIsHardware(in, "SvUIntToOH input")
    width match {
      case 0 => 0.U(0.W)
      case 1 => 1.U(1.W)
      case _ =>
        val decoder = Module(new SvUIntToOH(width))
        // Port assignment truncates or zero-extends, including Bool and inputs
        // whose width is inferred later, without relying on Bool.pad.
        decoder.io.in := in
        decoder.io.out
    }
  }
}
