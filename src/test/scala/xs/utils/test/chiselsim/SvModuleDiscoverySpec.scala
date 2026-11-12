package xs.utils.test.chiselsim

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.jar.{Attributes, JarEntry, JarOutputStream, Manifest}
import org.scalatest.funspec.AnyFunSpec
import scala.jdk.CollectionConverters._
import xs.utils.GlobalData
import xs.utils.verilog.{SvModuleMain, VlogRegistry}

class SvModuleDiscoverySpec extends AnyFunSpec {
  private val consumerPackage = "vlogscan.consumer."
  private val fixtureFiles = Seq(
    "Probe", "Probe$", "ConsumerFamily", "DefaultGenerator", "DefaultGenerator$",
    "PlainGenerator", "PlainGenerator$", "Container", "Container$", "Container$NestedGenerator$",
    "Unrelated", "Unrelated$", "NeedsEnclosingInstance", "NeedsEnclosingInstance$MemberGenerator$",
    "OptionalBase", "OptionalUnrelated", "OptionalUnrelated$"
  ).map(_ + ".class")

  private def withConsumer(jar: Boolean, manifestOnly: Boolean = false, missingOptional: Boolean = false)
    (test: (URLClassLoader, Path, Path) => Unit): Unit = {
    val temporary = Files.createTempDirectory("sv-classpath-consumer-")
    try {
      val classes = temporary.resolve("consumer classes")
      fixtureFiles.filterNot(name => missingOptional && name == "OptionalBase.class").foreach { name =>
        val entry = consumerPackage.replace('.', '/') + name
        val target = classes.resolve(entry)
        Files.createDirectories(target.getParent)
        val stream = getClass.getClassLoader.getResourceAsStream(entry)
        require(stream != null, s"Missing compiled fixture $entry")
        try Files.copy(stream, target)
        finally stream.close()
      }
      val artifact = if (jar) {
        val path = temporary.resolve("consumer library.jar")
        val output = new JarOutputStream(Files.newOutputStream(path))
        val files = Files.walk(classes)
        try files.iterator.asScala.filter(Files.isRegularFile(_)).toVector.sortBy(_.toString).foreach { file =>
          output.putNextEntry(new JarEntry(classes.relativize(file).toString.replace('\\', '/')))
          output.write(Files.readAllBytes(file))
          output.closeEntry()
        } finally {
          files.close()
          output.close()
        }
        path
      } else classes
      val root = if (manifestOnly) {
        val path = temporary.resolve("launcher.jar")
        val manifest = new Manifest
        manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
        manifest.getMainAttributes.put(Attributes.Name.CLASS_PATH, "consumer%20library.jar launcher.jar")
        new JarOutputStream(Files.newOutputStream(path), manifest).close()
        path
      } else artifact
      val loader = new URLClassLoader(Array(root.toUri.toURL), getClass.getClassLoader) {
        override protected def loadClass(name: String, resolve: Boolean): Class[_] = synchronized {
          val cls: Class[_] = if (name.startsWith(consumerPackage)) {
            Option(findLoadedClass(name)).getOrElse(findClass(name))
          } else super.loadClass(name, false)
          if (resolve) resolveClass(cls)
          cls
        }
      }
      try test(loader, root, temporary.resolve("sv"))
      finally loader.close()
    } finally os.remove.all(os.Path(temporary))
  }

  private def probe(loader: ClassLoader, field: String): Int = {
    val cls = Class.forName(consumerPackage + "Probe$", true, loader)
    cls.getMethod(field).invoke(cls.getField("MODULE$").get(null)).asInstanceOf[Integer].intValue()
  }

  private def flag(loader: ClassLoader, field: String): Unit = {
    val cls = Class.forName(consumerPackage + "Probe$", true, loader)
    cls.getMethod(field + "_$eq", java.lang.Boolean.TYPE)
      .invoke(cls.getField("MODULE$").get(null), java.lang.Boolean.TRUE)
  }

  private def text(path: Path): String = new String(Files.readAllBytes(path), UTF_8)

  describe("classpath SV provider discovery") {
    for (jar <- Seq(false, true)) {
      it(s"initializes only provider singletons from a consumer ${if (jar) "JAR" else "directory"}") {
        withConsumer(jar) { (loader, root, output) =>
          assert(probe(loader, "initialized") == 0)
          val modules = SvModuleMain.discover(Seq(root, root), loader)
          assert(modules.map(_.moduleName) == Seq("ScanConsumer7", "ScanNested", "ScanPlain"))
          assert(probe(loader, "initialized") == 3)
          assert(probe(loader, "defaultCalls") == 1)
          assert(probe(loader, "renderCalls") == 0)
          val sources = SvModuleMain.write(output, modules)
          assert(sources.head.moduleName == "SvPrefixReduceXOR")
          assert(!sources.exists(_.moduleName == "SvPrefixReduceAND"))
          assert(text(output.resolve("ScanConsumer7.sv")).contains("parameter integer WIDTH = 11"))
          assert(text(output.resolve("filelist.f")) == sources.map(_.fileName).mkString("", "\n", "\n"))
          sources.foreach(source => assert(text(output.resolve(source.fileName)) == source.text))
          assert(probe(loader, "renderCalls") == 1)
        }
      }
    }

    it("automatically scans caller URLs, parent classpaths, and dependencies through the context loader") {
      withConsumer(jar = true) { (loader, _, output) =>
        val thread = Thread.currentThread()
        val previous = thread.getContextClassLoader
        thread.setContextClassLoader(loader)
        try {
          val sources = SvModuleMain.exportAll(output)
          val names = sources.map(_.moduleName)
          assert(names.contains("ScanConsumer7"))
          assert(names.contains("SvPrefixReduceOR"))
          assert(names.contains("SvPrefixReduceXOR"))
          assert(names.contains("SvPrefixReduceAND"))
          assert(names.distinct == names)
          assert(probe(loader, "defaultCalls") == 1)
        } finally thread.setContextClassLoader(previous)
      }
    }

    it("follows manifest Class-Path entries, including escaped paths and cycles") {
      withConsumer(jar = true, manifestOnly = true) { (loader, root, _) =>
        val modules = SvModuleMain.discover(Seq(root), loader)
        assert(modules.map(_.moduleName) == Seq("ScanConsumer7", "ScanNested", "ScanPlain"))
        assert(probe(loader, "defaultCalls") == 1)
      }
    }

    it("supports explicit artifact roots with a non-URL class loader") {
      withConsumer(jar = true) { (loader, root, _) =>
        val opaque = new ClassLoader(loader) {}
        assert(SvModuleMain.discover(Seq(root), opaque).exists(_.moduleName == "ScanConsumer7"))
      }
    }

    it("does not reuse rendered sources or namespace prefixes between exports") {
      withConsumer(jar = false) { (loader, root, output) =>
        val previous = GlobalData.prefix
        try {
          GlobalData.prefix = "first_"
          val first = SvModuleMain.write(output, SvModuleMain.discover(Seq(root), loader))
          assert(SvModuleMain.write(output, SvModuleMain.discover(Seq(root), loader)) == first)
          Files.write(output.resolve("handwritten.sv"), "// keep\n".getBytes(UTF_8))
          GlobalData.prefix = "second_"
          val second = SvModuleMain.write(output, SvModuleMain.discover(Seq(root), loader))
          assert(second.forall(_.moduleName.startsWith("second_")))
          assert(text(output.resolve("second_ScanConsumer7.sv")).contains("second_SvPrefixReduceXOR"))
          assert(first.forall(source => !Files.exists(output.resolve(source.fileName))))
          assert(text(output.resolve("handwritten.sv")) == "// keep\n")
          assert(probe(loader, "initialized") == 3)
          assert(probe(loader, "defaultCalls") == 3)
        } finally GlobalData.prefix = previous
      }
    }

    it("skips an unrelated unloadable class with missing optional dependencies") {
      withConsumer(jar = true, missingOptional = true) { (loader, root, _) =>
        assert(SvModuleMain.discover(Seq(root), loader).exists(_.moduleName == "ScanConsumer7"))
      }
    }

    it("reports a provider initializer failure rather than silently dropping the provider") {
      withConsumer(jar = true) { (loader, root, _) =>
        flag(loader, "failInitialization")
        val error = intercept[IllegalArgumentException](SvModuleMain.discover(Seq(root), loader))
        assert(error.getMessage.contains("vlogscan.consumer.DefaultGenerator$"))
        assert(error.getCause.isInstanceOf[ExceptionInInitializerError])
      }
    }

    it("reports a provider default-generation failure") {
      withConsumer(jar = true) { (loader, root, _) =>
        flag(loader, "failDefaults")
        val error = intercept[IllegalArgumentException](SvModuleMain.discover(Seq(root), loader))
        assert(error.getMessage.contains("vlogscan.consumer.DefaultGenerator$"))
        assert(error.getCause.getMessage.contains("consumer defaults failed"))
      }
    }

    it("keeps conflicting definitions for registry validation before any file is written") {
      withConsumer(jar = false) { (loader, root, output) =>
        flag(loader, "conflict")
        val modules = SvModuleMain.discover(Seq(root), loader)
        val error = intercept[IllegalArgumentException](SvModuleMain.write(output, modules))
        assert(error.getMessage.contains("Conflicting SV definitions for ScanConsumer7"))
        assert(!Files.exists(output))
      }
    }

    it("fails clearly when no provider singletons exist in the selected roots") {
      val root = Files.createTempDirectory("empty-sv-classpath-")
      try {
        val error = intercept[IllegalArgumentException](SvModuleMain.discover(Seq(root), getClass.getClassLoader))
        assert(error.getMessage.contains("No VlogModuleProvider singletons"))
      } finally os.remove.all(os.Path(root))
    }

    it("falls back to the library loader when no context class loader is set") {
      val thread = Thread.currentThread()
      val previous = thread.getContextClassLoader
      try {
        thread.setContextClassLoader(null)
        val variants = SvModuleMain.discover().filter(_.moduleName.startsWith("SvPrefixReduce"))
        assert(variants.map(_.moduleName) == Seq("SvPrefixReduceAND", "SvPrefixReduceOR", "SvPrefixReduceXOR"))
        assert(new VlogRegistry().sources(variants).map(_.moduleName) == variants.map(_.moduleName))
      } finally thread.setContextClassLoader(previous)
    }
  }
}
