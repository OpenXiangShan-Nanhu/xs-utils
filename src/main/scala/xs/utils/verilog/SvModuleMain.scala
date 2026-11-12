package xs.utils.verilog

import java.io.File
import java.lang.reflect.Modifier
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.util.jar.JarFile
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/** Export the production SV templates without constructing Chisel hardware. */
object SvModuleMain {
  private def defaultClassLoader: ClassLoader =
    Option(Thread.currentThread().getContextClassLoader).getOrElse(getClass.getClassLoader)

  /** Collect ordinary JVM and URLClassLoader classpaths, including caller/dependency artifacts. */
  private def classpath(loader: ClassLoader): Seq[Path] = {
    val loaders = Iterator.iterate(loader)(_.getParent).takeWhile(_ != null).toVector
    val urls = loaders.collect { case urlLoader: URLClassLoader => urlLoader }
      .flatMap(_.getURLs) ++ loader.getResources("").asScala.toVector ++
      Option(classOf[VlogModuleProvider].getProtectionDomain.getCodeSource).map(_.getLocation)
    val systemPaths = if (loaders.contains(ClassLoader.getSystemClassLoader)) {
      System.getProperty("java.class.path", "").split(File.pathSeparator).toSeq.map(Paths.get(_))
    } else Seq.empty
    systemPaths ++ urls.filter(_.getProtocol == "file").map(url => Paths.get(url.toURI))
  }

  /** Discover providers without requiring prior singleton access or hardware instantiation. */
  def discover(): Seq[VlogModule] = discover(defaultClassLoader)

  def discover(loader: ClassLoader): Seq[VlogModule] = {
    require(loader != null, "SV discovery class loader must not be null")
    discover(classpath(loader), loader)
  }

  /** Explicit roots support custom class loaders and restricting export to selected artifacts.
    * Roots are class directories or JARs; manifest Class-Path dependencies are included.
    * The loader must see both these roots and the same VlogModuleProvider API as xs-utils.
    */
  def discover(roots: Seq[Path], loader: ClassLoader): Seq[VlogModule] = {
    require(loader != null, "SV discovery class loader must not be null")
    val artifacts = mutable.LinkedHashSet.empty[Path]
    val names = mutable.TreeSet.empty[String]

    def addClass(entry: String): Unit = {
      if (entry.endsWith("$.class") && !entry.startsWith("META-INF/")) {
        names += entry.stripSuffix(".class").replace('/', '.')
      }
    }

    def scan(root: Path): Unit = {
      if (Files.exists(root)) {
        val artifact = root.toRealPath()
        if (artifacts.add(artifact)) {
          if (Files.isDirectory(artifact)) {
            val paths = Files.walk(artifact)
            try paths.iterator.asScala.filter(Files.isRegularFile(_)).foreach { path =>
              addClass(artifact.relativize(path).toString.replace('\\', '/'))
            }
            finally paths.close()
          } else if (artifact.toString.toLowerCase(java.util.Locale.ROOT).endsWith(".jar")) {
            val jar = new JarFile(artifact.toFile)
            try {
              jar.entries().asScala.filterNot(_.isDirectory).foreach(entry => addClass(entry.getName))
              Option(jar.getManifest).flatMap(manifest =>
                Option(manifest.getMainAttributes.getValue("Class-Path"))).foreach { value =>
                value.trim.split("\\s+").filter(_.nonEmpty).foreach { entry =>
                  val uri = artifact.toUri.resolve(entry)
                  if (uri.getScheme == "file") scan(Paths.get(uri))
                }
              }
            } finally jar.close()
          }
        }
      }
    }
    roots.foreach(scan)

    val base = classOf[VlogModuleProvider]
    val modules = names.toVector.flatMap { name =>
      // Optional dependencies in unrelated libraries may be absent. Loading class metadata
      // never initializes an object; failures after identifying a provider are fatal below.
      val cls: Option[Class[_]] = try Some(Class.forName(name, false, loader))
      catch {
        case cause: ClassNotFoundException =>
          System.err.println(s"SV discovery: cannot load $name: $cause")
          None
        case cause: LinkageError =>
          System.err.println(s"SV discovery: cannot link $name: $cause")
          None
      }
      cls.filter(base.isAssignableFrom).toSeq.flatMap { providerClass =>
        // Local objects and objects inside ordinary classes need an enclosing instance.
        // Only independently accessible Scala singletons have their own static MODULE$.
        val singleton = try Some(providerClass.getDeclaredField("MODULE$"))
        catch { case _: NoSuchFieldException => None }
        singleton.filter(field => Modifier.isStatic(field.getModifiers) && field.getType == providerClass)
          .toSeq.flatMap { field =>
            try field.get(null).asInstanceOf[VlogModuleProvider].modules
            catch {
              case NonFatal(cause) => throw new IllegalArgumentException(s"Cannot initialize SV provider $name", cause)
              case cause: LinkageError => throw new IllegalArgumentException(s"Cannot initialize SV provider $name", cause)
            }
          }
      }
    }
    require(modules.nonEmpty, "No VlogModuleProvider singletons found in " + artifacts.mkString(", "))
    modules.sortBy(_.moduleName)
  }

  /** Export all discoverable providers and their dependencies without constructing hardware. */
  def exportAll(output: Path, loader: ClassLoader = defaultClassLoader): Seq[VlogSource] =
    write(output, discover(loader))

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
    val sources = exportAll(output)
    println(s"Generated ${sources.size} SV modules in $output")
  }
}
