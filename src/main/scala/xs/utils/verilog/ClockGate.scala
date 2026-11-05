package xs.utils.verilog

import chisel3._
import chisel3.experimental.BaseModule
import chisel3.util.experimental.BoringUtils
import xs.utils.constr.{GetCells, GetPins}

import scala.collection.mutable

class ClockGate extends VlogExtModule {
  val io = FlatIO(new Bundle {
    val TE = Input(Bool())
    val E = Input(Bool())
    val CK = Input(Clock())
    val Q = Output(Clock())
  })
  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(ClockGate)
  rtl.register(this, ClockGate)
}

case class ClockGateCellInfo(
  instPattern:String,
  clockIn:String,
  clockOut:String
) {
  def clockInPin(wrapper:() => String):GetPins = {
    GetPins(target = () => s"${wrapper()}/$instPattern/$clockIn")
  }
  def clockOutPin(wrapper:() => String):GetPins = {
    GetPins(target = () => s"${wrapper()}/$instPattern/$clockOut")
  }
}

class CgteBundle extends Bundle {
  val te = Input(Bool())
}
object ClockGate extends VlogModule {
  override val moduleName = "ClockGate"
  override def render(ctx: VlogContext): String =
    s"""
      |// VCS coverage exclude_file
      |module ${ctx.selfName} (
      |  input  wire TE,
      |  input  wire E,
      |  input  wire CK,
      |  output wire Q
      |);
      |`ifdef SYNTHESIS
      |  assign Q = CK;
      |`elsif BYPASS_CLOCKGATE
      |  assign Q = CK;
      |`else
      |  reg EN;
      |  always_latch begin
      |    if(!CK) EN = TE | E;
      |  end
      |  assign Q = CK & EN;
      |`endif
      |endmodule""".stripMargin

  private val teBoringQueue = new mutable.Queue[CgteBundle]
  private val hashModulesHasCgen = new mutable.Queue[BaseModule]
  var clockGateCellInfo = ClockGateCellInfo(
    instPattern = "U_STD_CLKGT",
    clockIn = "CP",
    clockOut = "Q"
  )

  def apply(TE:Bool, E:Bool, CK:Clock):Clock = {
    val module = Module.currentModule.get
    val cgbd = if(hashModulesHasCgen.contains(module)) {
      teBoringQueue.last
    } else {
      val cg = Wire(new CgteBundle)
      cg.te := TE
      dontTouch(cg)
      teBoringQueue.append(cg)
      hashModulesHasCgen.append(module)
      cg
    }
    val clockGate = Module(new ClockGate)
    clockGate.io.E := E || withClock(CK){ RegNext(E, true.B) }
    clockGate.io.TE := cgbd.te
    clockGate.io.CK := CK
    clockGate.io.Q
  }
  def getTop:CgteBundle = {
    val cgen = Wire(new CgteBundle)
    teBoringQueue.toSeq.foreach(BoringUtils.bore(_) := cgen)
    teBoringQueue.clear()
    cgen
  }
}