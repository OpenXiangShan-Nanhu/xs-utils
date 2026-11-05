package xs.utils.verilog

import scala.collection.mutable

/** A stateless, parameterized SV definition. Hardware instances are not created here.
  * SV templates use always_comb/always_ff with procedural loops for bulk assignments;
  * generate-for is reserved for repeated module instances. Reduction arrays must be
  * assigned in dependency order, without a self-accumulating temporary variable.
  */
trait VlogModule {
  def moduleName: String
  def render(ctx: VlogContext): String
}

final case class VlogSource(moduleName: String, text: String) {
  def fileName: String = s"$moduleName.sv"
}

/** Each render gets its own context; referring to a module also records its dependency. */
final class VlogContext private[verilog] (module: VlogModule, registry: VlogRegistry) {
  private[verilog] val dependencies = mutable.LinkedHashSet.empty[VlogModule]
  val selfName: String = registry.nameOf(module)

  def ref(dependency: VlogModule): String = {
    dependencies += dependency
    registry.nameOf(dependency)
  }
}

/** Captures GlobalData.prefix once so declarations and references use the same namespace.
  * Traversal state is local to each registration, not the JVM.
  */
final class VlogRegistry {
  private val prefix = xs.utils.GlobalData.prefix
  def nameOf(module: VlogModule): String = {
    val name = prefix + module.moduleName
    require(name.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"Invalid SV module name: $name")
    name
  }

  /** Dependency-first, deterministic closure; validate everything before emitting annotations. */
  def sources(root: VlogModule): Seq[VlogSource] = {
    sources(Seq(root))
  }

  /** Collect multiple roots with shared dependency and conflict checking. */
  def sources(roots: Seq[VlogModule]): Seq[VlogSource] = {
    val visited = mutable.HashSet.empty[VlogModule]
    val active = mutable.ArrayBuffer.empty[VlogModule]
    val emitted = mutable.LinkedHashMap.empty[String, VlogSource]

    def visit(module: VlogModule): Unit = {
      val cycleStart = active.indexWhere(nameOf(_) == nameOf(module))
      require(cycleStart < 0,
        s"Cyclic SV dependency: ${(active.drop(cycleStart max 0) :+ module).map(nameOf).mkString(" -> ")}")
      if (!visited(module)) {
        active += module
        val ctx = new VlogContext(module, this)
        val text = module.render(ctx)
        // firtool consumes a terminal newline when emitting verbatim sources.
        val source = VlogSource(ctx.selfName, text.stripSuffix("\n") + "\n\n")
        ctx.dependencies.foreach(visit)
        emitted.get(source.moduleName) match {
          case Some(previous) =>
            require(previous.text == source.text,
              s"Conflicting SV definitions for ${source.moduleName}")
          case None => emitted(source.moduleName) = source
        }
        active.remove(active.size - 1)
        visited += module
      }
    }

    roots.foreach(visit)
    emitted.values.toSeq
  }

  /** Register the root first, then its dependency closure. VlogExtModule shares
    * the emitted-source cache across all instances in this elaboration, so a
    * definition can never be emitted both as a primary source and as collateral.
    */
  def register(owner: VlogExtModule, root: VlogModule): Unit = {
    val closure = sources(root)
    closure.foreach(source => owner.validateInline(source.fileName, source.text))
    val ordered = closure.last +: closure.init
    ordered.foreach(source => owner.setInline(source.fileName, source.text))
  }
}
