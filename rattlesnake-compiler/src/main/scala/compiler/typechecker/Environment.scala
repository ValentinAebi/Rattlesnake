package compiler.typechecker

import compiler.typechecker.SubcaptureRelation.isCoveredBy
import lang.Capturables.Capturable
import lang.CaptureDescriptors.{CaptureDescriptor, CaptureSet, Mark}

sealed trait Environment {
  val insideEnclosure: Boolean

  def allowsCapturable(capturable: Capturable)(using TypeCheckingContext): Boolean

}

object RootEnvir extends Environment {
  override val insideEnclosure: Boolean = false

  override def allowsCapturable(capturable: Capturable)(using TypeCheckingContext): Boolean =
    capturable.isCoveredBy(CaptureSet.singletonOfRoot)
}

final class EnclosedEnvir(val enclosingEnvir: Environment) extends Environment {
  override val insideEnclosure: Boolean = true

  override def allowsCapturable(capturable: Capturable)(using TypeCheckingContext): Boolean = true
}

final class RestrictedEnvir(val cs: CaptureSet, val enclosingEnvir: Environment) extends Environment {
  export enclosingEnvir.insideEnclosure

  override def allowsCapturable(capturable: Capturable)(using TypeCheckingContext): Boolean =
    capturable.isCoveredBy(cs)

}
