package compiler.typechecker

import lang.CaptureDescriptors.*
import lang.{CaptureDescriptors, Types}
import lang.Capturables.Capturable
import lang.Types.*

import scala.collection.mutable


trait CapturesSubstitutor {
  
  def subst(capturable: Capturable): Capturable

  def subst(tpe: Type): Type = tpe match {
    case CapturingType(shape, captureDescriptor) => CapturingType(subst(shape), subst(captureDescriptor))
    case shape: TypeShape => subst(shape)
  }

  def subst(shape: TypeShape): TypeShape = shape match {
    case primOrNamed: (PrimitiveTypeShape | NamedTypeShape) => primOrNamed
    case ArrayTypeShape(elemType, modifiable) => ArrayTypeShape(subst(elemType), modifiable)
    case UnionTypeShape(unitedTypes) => UnionTypeShape(unitedTypes.map(subst))
    case UndefinedTypeShape => UndefinedTypeShape
  }
  
  def subst(captureDescriptor: CaptureDescriptor): CaptureDescriptor = captureDescriptor match {
    case Brand => Brand
    case CaptureSet(set) => CaptureSet(set.map(subst))
  }

}

object CapturesSubstitutor {
  
  def from(substMap: Map[Capturable, Capturable]): CapturesSubstitutor =
    (capturable: Capturable) => substMap.getOrElse(capturable, capturable)
    
  def fromView(substMap: mutable.Map[Capturable, Capturable]): CapturesSubstitutor =
    (capturable: Capturable) => substMap.getOrElse(capturable, capturable)

}
