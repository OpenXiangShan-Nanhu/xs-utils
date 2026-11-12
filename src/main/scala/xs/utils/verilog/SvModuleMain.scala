package xs.utils.verilog

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters._

/** Export the production SV templates without constructing Chisel hardware. */
object SvModuleMain {
  /** Inspect only this library's production artifact, not test/dependency classpaths.
    * Loading is deferred until a class is known to be a VlogModuleProvider singleton.
    */
  def discover(): Seq[VlogModule] = {
    val base = classOf[VlogModuleProvider]
    val artifact = Paths.get(base.getProtectionDomain.getCodeSource.getLocation.toURI)
    val entries = if (Files.isDirectory(artifact)) {
      val paths = Files.walk(artifact.resolve("xs/utils/verilog"))
      try paths.iterator.asScala.filter(Files.isRegularFile(_))
        .map(path => artifact.relativize(path).toString.replace('\\', '/')).toVector
      finally paths.close()
    } else {
      val jar = new JarFile(artifact.toFile)
      try jar.entries().asScala.map(_.getName).toVector
      finally jar.close()
    }
    val modules = entries.filter(name =>
      name.startsWith("xs/utils/verilog/") && name.endsWith("$.class")
    ).sorted.flatMap { name =>
      val cls = Class.forName(name.stripSuffix(".class").replace('/', '.'), false, base.getClassLoader)
      if (base.isAssignableFrom(cls)) {
        cls.getField("MODULE$").get(null).asInstanceOf[VlogModuleProvider].modules
      } else Seq.empty
    }
    require(modules.nonEmpty, "No VlogModule templates found in " + artifact)
    modules.sortBy(_.moduleName)
  }

  /** Keep parameter declarations intact; downstream tools use their SV defaults.
    * Only stale files named in our previous manifest are removed.
    */
  def write(output: Path, modules: Seq[VlogModule]): Seq[VlogSource] = {
    val sources = new VlogRegistry().sources(modules)
    require(sources.nonEmpty, "No SV sources to export")
    val manifest = output.resolve("filelist.f")
    val previous = if (Files.exists(manifest)) Files.readAllLines(manifest, UTF_8).asScala.toSeq else Seq.empty
    require(previous.forall(_.matches("[A-Za-z_][A-Za-z0-9_$]*\\.sv")),
      "Invalid generated filelist: " + manifest)
    Files.createDirectories(output)
    sources.foreach(source => Files.write(output.resolve(source.fileName), source.text.getBytes(UTF_8)))
    val names = sources.map(_.fileName)
    previous.filterNot(names.contains).foreach(name => Files.deleteIfExists(output.resolve(name)))
    Files.write(manifest, (names.mkString("\n") + "\n").getBytes(UTF_8))
    sources
  }

  def main(args: Array[String]): Unit = {
    require(args.length <= 1, "Usage: SvModuleMain [output-directory]")
    val output = Paths.get(args.headOption.getOrElse("build/svmodule"))
    val sources = write(output, discover())
    println(s"Generated ${sources.size} SV modules in $output")
  }
}
