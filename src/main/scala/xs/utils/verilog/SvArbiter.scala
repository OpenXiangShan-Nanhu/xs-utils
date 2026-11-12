package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance}
import chisel3.util.{ArbiterIO, Decoupled, log2Ceil}

/** Flat SV boundary for a low-index-priority Decoupled arbiter.
  * Lane zero occupies the least-significant DATA_WIDTH bits.
  */
class SvArbiterImpl(dataWidth: Int = 32, inputSize: Int = 2) extends VlogExtModule(Map(
  "DATA_WIDTH" -> dataWidth,
  "INPUT_SIZE" -> inputSize
)) {
  require(dataWidth > 0, "SvArbiter DATA_WIDTH must be positive")
  require(inputSize > 0, "SvArbiter INPUT_SIZE must be positive")

  val io = IO(new Bundle {
    // Packed lane masks share the parameterized SV boundary; each bit is one lane.
    val in = new Bundle {
      val valid = Input(UInt(inputSize.W))
      val bits = Input(UInt((dataWidth * inputSize).W))
      val ready = Output(UInt(inputSize.W))
    }
    val out = Decoupled(UInt(dataWidth.W))
    val chosen = Output(UInt(math.max(1, log2Ceil(inputSize)).W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvArbiterImpl)
  rtl.register(this, SvArbiterImpl)
}

object SvArbiterImpl extends VlogModule {
  override val moduleName: String = "SvArbiter"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer DATA_WIDTH = 32,
       |  parameter integer INPUT_SIZE = 2
       |) (
       |  input  wire  [INPUT_SIZE - 1:0]                                   io_in_valid,
       |  input  wire  [INPUT_SIZE * DATA_WIDTH - 1:0]                      io_in_bits,
       |  output wire  [INPUT_SIZE - 1:0]                                   io_in_ready,
       |  output wire                                                       io_out_valid,
       |  output wire  [DATA_WIDTH - 1:0]                                   io_out_bits,
       |  input  wire                                                       io_out_ready,
       |  output wire  [((INPUT_SIZE > 1) ? $$clog2(INPUT_SIZE) : 1) - 1:0] io_chosen
       |);
       |  localparam integer CHOSEN_WIDTH = (INPUT_SIZE > 1) ? $$clog2(INPUT_SIZE) : 1;
       |
       |  // Each ready lane sees only higher-priority requests, even when invalid.
       |  wire [INPUT_SIZE - 1:0] valid_prefix;
       |  wire [CHOSEN_WIDTH - 1:0] first_valid;
       |  logic [INPUT_SIZE - 1:0] chosen_oh;
       |
       |  // Chisel equivalent: valid_prefix := VecInit(io_in_valid.asBools.scanLeft(false.B)(_ || _).tail).asUInt
       |  //   io_in_ready := Fill(INPUT_SIZE, io_out_ready) & ~(valid_prefix << 1)(INPUT_SIZE - 1, 0)
       |  // Shift the inclusive prefix to exclude each lane itself; zero-fill lane zero, including INPUT_SIZE=1.
       |  ${ctx.ref(SvPrefixReduceImpl(0))} #(.INPUT_SIZE(INPUT_SIZE), .HIGH_PREFIX(0)) u_valid_prefix (
       |    .io_in(io_in_valid), .io_out(valid_prefix)
       |  );
       |  assign io_in_ready = {INPUT_SIZE{io_out_ready}} & ~(valid_prefix << 1);
       |  assign io_out_valid = |io_in_valid;
       |
       |  ${ctx.ref(SvPriorityEncoderImpl)} #(.INPUT_SIZE(INPUT_SIZE)) u_choice (
       |    .io_in(io_in_valid), .io_out(first_valid)
       |  );
       |  // Native Arbiter selects the last lane when idle; the encoder returns zero.
       |  assign io_chosen = io_out_valid ? first_valid : CHOSEN_WIDTH'(INPUT_SIZE - 1);
       |
       |  // Chisel equivalent: chosen_oh := VecInit((0 until INPUT_SIZE).map(io_chosen === _.U)).asUInt
       |  //   io_out_bits := Mux1H(chosen_oh, io_in_bits.asTypeOf(Vec(INPUT_SIZE, UInt(DATA_WIDTH.W))))
       |  // io_chosen always names a lane, defaulting to the last lane when idle.
       |  always_comb begin
       |    for (integer i = 0; i < INPUT_SIZE; i = i + 1) begin : gen_chosen_oh
       |      chosen_oh[i] = io_chosen == CHOSEN_WIDTH'(i);
       |    end
       |  end
       |  ${ctx.ref(SvMux1HImpl)} #(.DATA_WIDTH(DATA_WIDTH), .INPUT_SIZE(INPUT_SIZE)) u_out_bits (
       |    .io_sel(chosen_oh),
       |    .io_in(io_in_bits),
       |    .io_out(io_out_bits)
       |  );
       |endmodule
       |""".stripMargin
}

/** Typed low-index-priority arbiter. A single input is wired through directly;
  * for multiple inputs, only the parameterized SV instance survives RTL emission.
  */
class SvArbiter[T <: Data](gen: T, n: Int) extends RawModule {
  require(n > 0, "SvArbiter requires at least one input")

  // A bypass can be elaborated before any SV core. Keep its Chisel module name
  // distinct from the core defname even before firtool applies inlineInstance.
  override def desiredName: String = "SvArbiterInline"

  val io = IO(new ArbiterIO(gen, n))

  if (n == 1) {
    io.out <> io.in.head
    io.chosen := 0.U
  } else {
    require(gen.isWidthKnown && gen.getWidth > 0,
      "SvArbiter requires a known, positive data width when n > 1")
    val core = Module(new SvArbiterImpl(gen.getWidth, n))
    core.io.in.valid := VecInit(io.in.map(_.valid)).asUInt
    io.in.map(_.ready).zip(core.io.in.ready.asBools).foreach {
      case (ready, grant) => ready := grant
    }
    core.io.in.bits := VecInit(io.in.map(_.bits)).asUInt
    io.out.valid := core.io.out.valid
    io.out.bits := core.io.out.bits.asTypeOf(io.out.bits)
    core.io.out.ready := io.out.ready
    io.chosen := core.io.chosen
  }

  inlineInstance(this)
}

object SvArbiter {
  /** Returns the arbiter interface. n == 1 is a Wire bypass: neither the inline
    * wrapper nor an SV core is constructed, and payload widths may be inferred.
    */
  def apply[T <: Data](gen: T = UInt(32.W), n: Int = 2)(implicit sourceInfo: SourceInfo): ArbiterIO[T] = {
    require(n > 0, "SvArbiter requires at least one input")
    if (n == 1) {
      val io = Wire(new ArbiterIO(gen, n))
      io.out <> io.in.head
      io.chosen := 0.U
      io
    } else {
      Module(new SvArbiter(gen, n)).io
    }
  }
}
