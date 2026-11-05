package xs.utils.verilog

import chisel3.{ExtModule, Param, VlogSourceCache}

/** Base for inline SV: each source is attached only once per elaboration,
  * whether first encountered as a direct ExtModule or as an SV dependency.
  */
abstract class VlogExtModule(params: Map[String, Param] = Map.empty) extends ExtModule(params) {
  private[verilog] final def validateInline(fileName: String, text: String): Unit = {
    VlogSourceCache.sources.get(fileName).foreach { previous =>
      require(previous == text, s"Conflicting SV definitions for $fileName")
    }
  }

  final override def setInline(fileName: String, text: String): Unit = {
    val sources = VlogSourceCache.sources
    validateInline(fileName, text)
    if (!sources.contains(fileName)) {
      super.setInline(fileName, text)
      sources(fileName) = text
    }
  }
}
