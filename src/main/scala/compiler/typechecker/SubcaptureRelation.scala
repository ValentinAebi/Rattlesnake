package compiler.typechecker

import lang.Types.Type
import lang.{Brand, CapturableValue, CaptureDescriptor, CaptureSet, SelectValue}

object SubcaptureRelation {

  extension (l: CaptureDescriptor) def subcaptureOf(r: CaptureDescriptor)(using ctx: TypeCheckingContext): Boolean = {
    (l, r) match {
      case (Brand, Brand) => true
      case (lcs: CaptureSet, rcs: CaptureSet) =>
        lcs.subcaptureOf(rcs)
    }
  }

  extension (l: CaptureSet) def subcaptureOf(r: CaptureSet)(using ctx: TypeCheckingContext): Boolean =
    l.set.forall(_.isCoveredBy(r))

  extension (l: CapturableValue) private def isCoveredBy(r: CaptureSet)(using ctx: TypeCheckingContext): Boolean = l match {
    case lPath if r.set.contains(lPath) => true
    case SelectValue(root, field) if root.isCoveredBy(r) => true
    case lPath if ctx.lookup(lPath).captureDescriptor.subcaptureOf(r) => true
    case _ => false
  }

}
