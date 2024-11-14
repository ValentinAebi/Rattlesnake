package lang

import Captures.*
import Types.*
import compiler.typechecker.TypeCheckingContext
import identifiers.{FunOrVarId, TypeIdentifier}

object SubcaptureRelation {

  extension (l: CaptureDescriptor) def subcaptureOf(r: CaptureDescriptor)(using ctx: SubcapturingContext): Boolean = (l, r) match {
    case (Brand, Brand) => true
    case (CaptureSet(lcs), CaptureSet(rcs)) =>
      if (lcs.size == 1) {
        lcs.head match {
          // Sc-elem-algo
          case p if rcs.contains(p) => true
          // Sc-path-algo
          case p: ProperPath
            if ctx.pathLookup(p)
              .flatMap(captureDescrOf)
              .exists(_.subcaptureOf(r))
          => true
          // Sc-mem-algo
          case SelectPath(lhs, select) if CaptureSet(lhs).subcaptureOf(r) => true
          case _ => false
        }
      } else {
        // Sc-set-algo
        lcs.forall(c => CaptureSet(c).subcaptureOf(r))
      }
    case _ => false
  }
  
  final case class SubcapturingContext(roots: Map[FunOrVarId, Type], structs: Map[TypeIdentifier, StructSignature]) {
    def pathLookup(p: ProperPath): Option[Type] = p match {
      case VarPath(root) => roots.get(root)
      case SelectPath(lhs, field) =>
        pathLookup(lhs).flatMap {
          case NamedType(typeName, modifiable, cDescr) if !modifiable =>
            for {
              sig <- structs.get(typeName)
              fieldInfo <- sig.fields.get(field)
              if !fieldInfo.isReassignable
            } yield fieldInfo.tpe
          case _ => None
        }
    }
  }
  
  private def captureDescrOf(tpe: Type): Option[CaptureDescriptor] = tpe match {
    case NamedType(typeName, modifiable, captureDescr) => Some(captureDescr)
    case _ => None
  }

}
