package top

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import xs.utils._
import chisel3.stage.ChiselGeneratorAnnotation
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import scala.annotation.tailrec
import scala.reflect.runtime.universe._

case object OptKey extends Field[Opt]

case class Opt(module: String = "", build: String = "build")

class DefaultConfig extends Config((site, here, up) => {
  case OptKey => Opt()
})

object Parser {

  def apply(args: Array[String]): (Parameters, Array[String]) = {
    val defaultConfig = new DefaultConfig
    var firrtlOpts = Array[String]()
    var hasHelp: Boolean = false
    var hasTd: Boolean = false

    @tailrec
    def parse(config: Parameters, args: List[String]): Parameters = {
      args match {
        case Nil => config

        case "--help" :: tail =>
          println(
            """
              |Customized Options
              |  --module <full class name>
          """.stripMargin)
          hasHelp = true
          parse(config, tail)

        case "--module" :: confStr :: tail =>
          parse(config.alter((site, here, up) => {
            case OptKey => up(OptKey).copy(module = confStr)
          }), tail)

        case "-td" :: bdStr :: tail =>
          hasTd = true
          parse(config.alter((site, here, up) => {
            case OptKey => up(OptKey).copy(build = bdStr)
          }), tail)

        case "--prefix" :: pfxStr :: tail =>
          GlobalData.prefix = pfxStr
          PrefixPhase.prefix = pfxStr
          parse(config, tail)

        case option :: tail =>
          firrtlOpts :+= option
          parse(config, tail)
      }
    }

    val cfg = parse(defaultConfig, args.toList)
    if (hasHelp) firrtlOpts :+= "--help"
    if (hasTd) {
      firrtlOpts :+= "-td"
      firrtlOpts :+= cfg(OptKey).build
    }
    (cfg, firrtlOpts)
  }
}

object TestTop extends App {
  val (config, firrtlOpts) = Parser(args)
  val module = if (config(OptKey).module == "") "dft.OCC" else config(OptKey).module
  lazy val m = Class.forName("xs.utils." + module).getDeclaredConstructor().newInstance().asInstanceOf[RawModule]
  (new XsStage).execute(firrtlOpts, Seq(
    FirtoolOption("-O=release"),
    FirtoolOption("--disable-all-randomization"),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--strip-debug-info"),
    FirtoolOption("--lower-memories"),
    FirtoolOption("--add-vivado-ram-address-conflict-synthesis-bug-workaround"),
    FirtoolOption("--lowering-options=noAlwaysComb," +
      " disallowPortDeclSharing, disallowLocalVariables," +
      " emittedLineLength=120, explicitBitcast, locationInfoStyle=plain," +
      " disallowExpressionInliningInPorts, disallowMuxInlining"),
    ChiselGeneratorAnnotation(() => m)
  ))
  val mod = GlobalData.prefix + module.split("\\.").last
  dft.FileManager.writeOut(mod)
  FileRegisters.write("build", mod + ".")
}