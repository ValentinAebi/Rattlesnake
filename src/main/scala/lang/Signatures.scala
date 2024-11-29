package lang

import identifiers.*
import lang.CaptureDescriptors.{CaptureDescriptor, CaptureSet}
import lang.StructSignature.FieldInfo
import lang.Types.PrimitiveTypeShape.VoidType
import lang.Types.{NamedTypeShape, Type}

import java.util
import scala.collection.mutable

final case class FunctionSignature(name: FunOrVarId, args: List[(Option[FunOrVarId], Type)], retType: Type)

sealed trait TypeSignature {
  def id: TypeIdentifier

  def isInterface: Boolean

  def isModuleOrPackage: Boolean

  def functions: Map[FunOrVarId, FunctionSignature]

  def typeOfSelectIfCapturable(sel: FunOrVarId): Option[Type]

  def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo]
  
  def getCaptureDescr: CaptureDescriptor
}

sealed trait ModuleOrPackageSignature extends TypeSignature {

  def paramImports: mutable.LinkedHashMap[FunOrVarId, Type]

  def importedPackages: mutable.LinkedHashSet[TypeIdentifier]

  def importedDevices: mutable.LinkedHashSet[Device]

  override def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo] =
    paramImports.map((id, tpe) => id -> FieldInfo(tpe, isReassignable = false))

  override def isInterface: Boolean = false

  override def isModuleOrPackage: Boolean = true

  override def typeOfSelectIfCapturable(sel: FunOrVarId): Option[Type] = paramImports.get(sel)
}

sealed trait StructOrModuleSignature extends TypeSignature {
  def constructorSig: FunctionSignature =
    FunctionSignature(ConstructorFunId, params.toList.map((id, info) => (Some(id), info.tpe)), VoidType)

  def isShallowMutable: Boolean
}

final case class ModuleSignature(
                                  id: TypeIdentifier,
                                  paramImports: mutable.LinkedHashMap[FunOrVarId, Type],
                                  importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                  importedDevices: mutable.LinkedHashSet[Device],
                                  functions: Map[FunOrVarId, FunctionSignature]
                                ) extends ModuleOrPackageSignature, StructOrModuleSignature {

  override def getCaptureDescr: CaptureDescriptor = CaptureSet((
    paramImports.map((paramId, _) => MePath.dot(paramId)) ++
      importedPackages.map(CapPackage(_)) ++
      importedDevices.map(CapDevice(_))
    ).toSet)

  override def isShallowMutable: Boolean = false
}

final case class PackageSignature(
                                   id: TypeIdentifier,
                                   importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                   importedDevices: mutable.LinkedHashSet[Device],
                                   functions: Map[FunOrVarId, FunctionSignature]
                                 ) extends ModuleOrPackageSignature {

  override def paramImports: mutable.LinkedHashMap[FunOrVarId, Type] = mutable.LinkedHashMap.empty

  override def getCaptureDescr: CaptureDescriptor = CaptureSet((
    importedPackages.map(CapPackage(_)) ++
      importedDevices.map(CapDevice(_))
    ).toSet)
}

final case class StructSignature(
                                  id: TypeIdentifier,
                                  fields: mutable.LinkedHashMap[FunOrVarId, FieldInfo],
                                  directSupertypes: Seq[TypeIdentifier],
                                  isInterface: Boolean
                                ) extends StructOrModuleSignature {
  override def isModuleOrPackage: Boolean = false

  override def functions: Map[FunOrVarId, FunctionSignature] = Map.empty

  override def params: mutable.LinkedHashMap[FunOrVarId, FieldInfo] = fields

  override def isShallowMutable: Boolean = fields.exists(_._2.isReassignable)

  override def typeOfSelectIfCapturable(sel: FunOrVarId): Option[Type] =
    fields.get(sel).filter(!_.isReassignable).map(_.tpe)

  override def getCaptureDescr: CaptureDescriptor = CaptureSet(
    fields.filter((_, info) => !info.tpe.captureDescriptor.isEmpty).map((id, _) => MePath.dot(id)).toSet
  )
}

final case class DeviceSignature(
                                  id: TypeIdentifier,
                                  functions: Map[FunOrVarId, FunctionSignature]
                                ) extends TypeSignature {

  override def isInterface: Boolean = false

  override def isModuleOrPackage: Boolean = false

  override def getCaptureDescr: CaptureDescriptor = CaptureSet.singletonOfRoot
}

object StructSignature {
  final case class FieldInfo(tpe: Type, isReassignable: Boolean)
}
