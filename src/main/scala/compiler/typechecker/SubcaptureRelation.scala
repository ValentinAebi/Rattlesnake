package compiler.typechecker

import lang.Capturables.*
import lang.CaptureDescriptors.*

object SubcaptureRelation {

  extension (l: CaptureDescriptor) def subcaptureOf(r: CaptureDescriptor)(using ctx: TypeCheckingContext): Boolean = {
    (l, r) match {
      case (Brand, Brand) => true
      case (lcs: CaptureSet, rcs: CaptureSet) =>
        lcs.subcaptureOf(rcs)
      case _ => false
    }
  }

  extension (l: CaptureSet) def subcaptureOf(r: CaptureSet)(using ctx: TypeCheckingContext): Boolean =
    l.set.forall(_.isCoveredBy(r))

  extension (l: Capturable) private def isCoveredBy(r: CaptureSet)(using ctx: TypeCheckingContext): Boolean = l match {
    case lPath if r.set.contains(lPath) => true
    case SelectPath(root, field) if root.isCoveredBy(r) => true
    case IdPath(id) if ctx.getLocalOnly(id).exists(_.tpe.captureDescriptor.subcaptureOf(r)) => true
    case _ => false
  }

}
