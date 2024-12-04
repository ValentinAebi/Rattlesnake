package compiler.typechecker

import compiler.pipeline.CompilationStep.TypeChecking
import compiler.reporting.Errors.{Err, ErrorReporter}
import compiler.reporting.Position
import lang.Capturables.*
import lang.CaptureDescriptors.*
import lang.Types.*
import lang.{Capturables, CaptureDescriptors, Types}

import scala.collection.mutable

final class PathsSubstitutor(tcCtx: TypeCheckingContext, er: ErrorReporter) {
  private val substMap: mutable.Map[Path, CaptureDescriptor] = mutable.Map.empty

  export substMap.update

  def subst(capturable: Capturable, posOpt: Option[Position]): CaptureDescriptor = capturable match {
    case path: Capturables.Path =>
      substMap.getOrElse(path, {
        er.push(Err(TypeChecking, s"unexpected path '$path' in capture set", posOpt))
        CaptureSet.singletonOfRoot
      })
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
    case Brand => Brand
    case CaptureSet(set) => set.foldLeft(CaptureDescriptors.emptyCaptureSet) {
      (cd, capturable) => cd.union(subst(capturable, posOpt))
    }
  }

}
