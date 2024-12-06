package lang

import identifiers.*
import lang.Capturables.*
import lang.CaptureDescriptors.{CaptureDescriptor, CaptureSet}
import lang.Types.PrimitiveTypeShape.VoidType
import lang.Types.{NamedTypeShape, Type}

import java.util
import scala.collection.mutable

final case class FunctionSignature(name: FunOrVarId, args: List[(Option[FunOrVarId], Type)], retType: Type)

sealed trait TypeSignature {
  def id: TypeIdentifier

  def getNonSubstitutedCaptureDescr: CaptureDescriptor

  def getNonSubstitutedType: Type = NamedTypeShape(id) ^ getNonSubstitutedCaptureDescr

  def isInterface: Boolean
}

sealed trait FunctionsProviderSig extends TypeSignature {
  def functions: Map[FunOrVarId, FunctionSignature]
}

sealed trait ConstructibleSig extends TypeSignature {

  def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo]

  def voidInitMethodSig: FunctionSignature =
    FunctionSignature(ConstructorFunId, params.toList.map((id, info) => (Some(id), info.tpe)), VoidType)
}

sealed trait UserConstructibleSig extends TypeSignature {
  this: ConstructibleSig =>
}

sealed trait SelectableSig extends TypeSignature {
  this: ConstructibleSig =>

  def typeOfSelectIfCapturable(sel: FunOrVarId): Option[Type] =
    params.get(sel).filter(!_.isReassignable).map(_.tpe)
}

sealed trait ImporterSig extends TypeSignature {

  def paramImports: mutable.LinkedHashMap[FunOrVarId, Type]

  def importedPackages: mutable.LinkedHashSet[TypeIdentifier]

  def importedDevices: mutable.LinkedHashSet[Device]

  def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo] =
    paramImports.map((id, tpe) => id -> FieldInfo(tpe, isReassignable = false))
}

final case class ModuleSignature(
                                  id: TypeIdentifier,
                                  paramImports: mutable.LinkedHashMap[FunOrVarId, Type],
                                  importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                  importedDevices: mutable.LinkedHashSet[Device],
                                  functions: Map[FunOrVarId, FunctionSignature]
                                )
  extends TypeSignature, ConstructibleSig, UserConstructibleSig, ImporterSig, SelectableSig, FunctionsProviderSig {

  override def getNonSubstitutedCaptureDescr: CaptureDescriptor = CaptureSet((
    paramImports.map((paramId, _) => MePath.dot(paramId)) ++
      importedPackages.map(CapPackage(_)) ++
      importedDevices.map(CapDevice(_))
    ).toSet)

  override def isInterface: Boolean = false
}

final case class PackageSignature(
                                   id: TypeIdentifier,
                                   importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                   importedDevices: mutable.LinkedHashSet[Device],
                                   functions: Map[FunOrVarId, FunctionSignature]
                                 ) extends TypeSignature, ConstructibleSig, ImporterSig, FunctionsProviderSig {

  override def paramImports: mutable.LinkedHashMap[FunOrVarId, Type] = mutable.LinkedHashMap.empty

  override def getNonSubstitutedCaptureDescr: CaptureDescriptor = CaptureSet((
    importedPackages.map(CapPackage(_)) ++
      importedDevices.map(CapDevice(_))
    ).toSet)

  override def isInterface: Boolean = false
}

final case class StructSignature(
                                  id: TypeIdentifier,
                                  fields: mutable.LinkedHashMap[FunOrVarId, FieldInfo],
                                  directSupertypes: Seq[TypeIdentifier],
                                  isInterface: Boolean
                                )
  extends TypeSignature, ConstructibleSig, UserConstructibleSig, SelectableSig {

  override def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo] = fields

  def isShallowMutable: Boolean = fields.exists(_._2.isReassignable)

  override def getNonSubstitutedCaptureDescr: CaptureDescriptor = CaptureSet(
    fields.filter((_, info) => !info.tpe.captureDescriptor.isEmpty).map((id, _) => MePath.dot(id)).toSet
  )
}

final case class FieldInfo(tpe: Type, isReassignable: Boolean)
