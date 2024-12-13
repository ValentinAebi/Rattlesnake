package compiler.typechecker

import compiler.reporting.Errors.ErrorReporter
import compiler.reporting.Position
import lang.Capturables.*
import lang.CaptureDescriptors.*
import lang.Types.*
import lang.{Capturables, CaptureDescriptors, Types}

import scala.collection.mutable

final class PathsSubstitutor(tcCtx: TypeCheckingContext, er: ErrorReporter) {
  private val substMap: mutable.Map[Path, Capturable] = mutable.Map.empty

  export substMap.update
  
  def subst(path: Path): Option[Capturable] = path match {
    case path: (IdPath | MePath.type) =>
      substMap.get(path)
    case SelectPath(directRoot, fld) =>
      subst(directRoot) match {
        case Some(substDirectRoot: Path) => Some(substDirectRoot.dot(fld))
        case _ => None
      }
  }

  def subst(capturable: Capturable, posOpt: Option[Position]): CaptureDescriptor = capturable match {
    case path: Capturables.Path =>
      subst(path).map(CaptureSet(_))
        .getOrElse(tcCtx.lookup(path).captureDescriptor)
    case other: (CapPackage | CapDevice | RootCapability.type) => CaptureSet(other)
  }

  def subst(tpe: Type, posOpt: Option[Position]): Type = tpe match {
    case CapturingType(shape, captureDescriptor) => CapturingType(subst(shape, posOpt), subst(captureDescriptor, posOpt))
    case shape: TypeShape => subst(shape, posOpt)
  }

  def subst(shape: TypeShape, posOpt: Option[Position]): TypeShape = shape match {
    case primOrNamed: (PrimitiveTypeShape | NamedTypeShape) => primOrNamed
    case ArrayTypeShape(elemType, modifiable) => ArrayTypeShape(subst(elemType, posOpt), modifiable)
    case UnionTypeShape(unitedTypes) => UnionTypeShape(unitedTypes.map(subst(_, posOpt)))
    case UndefinedTypeShape => UndefinedTypeShape
  }

  def subst(captureDescriptor: CaptureDescriptor, posOpt: Option[Position]): CaptureDescriptor = captureDescriptor match {
    case Mark => Mark
    case CaptureSet(set) => set.foldLeft(CaptureDescriptors.emptyCaptureSet) {
      (cd, capturable) => cd.union(subst(capturable, posOpt))
    }
  }

}
