package circt

import firrtl.annotations.{Annotation, ModuleTarget, Named, SingleTargetAnnotation}

case class FullResetAnnotation(
	target: Named,
	resetType: String
) extends SingleTargetAnnotation[Named] {
	override def duplicate(n: Named): Annotation = this.copy(n)
}