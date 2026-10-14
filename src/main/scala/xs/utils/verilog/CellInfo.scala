package xs.utils.verilog

import xs.utils.sram.SramCellInfo

object CellInfo {
  def setProcessNode(pn:String):Unit = {
    pn match {
      case "TSMC12" =>
        xs.utils.sram.SramConstr.spSramInfo = SramCellInfo(
          instPattern = "U_sram_*",
          clockPort = "CLK*",
          inputPorts = Seq("D*", "A*", "WEB*", "CEB*", "BWEB*"),
          outputPort = "Q*"
        )
        xs.utils.sram.SramConstr.dpSramInfo = SramCellInfo(
          instPattern = "U_sram_*",
          clockPort = "CLK*",
          inputPorts = Seq("D*", "AA*", "AB*", "WEB*", "BWEB*", "REB*"),
          outputPort = "Q*"
        )
        xs.utils.ClockGate.clockGateCellInfo = ClockGateCellInfo(
          instPattern = "U_STD_CLKGT",
          clockIn = "CP",
          clockOut = "Q"
        )
    }
  }
}
