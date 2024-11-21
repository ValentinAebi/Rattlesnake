package lang

import identifiers.*
import lang.StructSignature.FieldInfo
import lang.Types.PrimitiveType.VoidType
import lang.Types.{NamedType, Type}

import java.util
import scala.collection.mutable

final case class FunctionSignature(
                                    name: FunOrVarId,
                                    argTypes: List[Type],
                                    retType: Type
                                  )

sealed trait TypeSignature {
  def id: TypeIdentifier
  def isInterface: Boolean
  def isModuleOrPackage: Boolean
  def functions: Map[FunOrVarId, FunctionSignature]
}

sealed trait ModuleOrPackageSignature extends TypeSignature {
  
  def paramImports: mutable.LinkedHashMap[FunOrVarId, Type]
  def importedPackages: mutable.LinkedHashSet[TypeIdentifier]
  def importedDevices: mutable.LinkedHashSet[Device]
  
  override def isInterface: Boolean = false
  override def isModuleOrPackage: Boolean = true
}

sealed trait StructOrModuleSignature extends TypeSignature {
  def constructorSig: FunctionSignature
  def isShallowMutable: Boolean
}

final case class ModuleSignature(
                                  id: TypeIdentifier,
                                  paramImports: mutable.LinkedHashMap[FunOrVarId, Type],
                                  importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                  importedDevices: mutable.LinkedHashSet[Device],
                                  functions: Map[FunOrVarId, FunctionSignature]
                                ) extends ModuleOrPackageSignature, StructOrModuleSignature {
  override def constructorSig: FunctionSignature =
    FunctionSignature(ConstructorFunId, paramImports.values.toList, VoidType)

  override def isShallowMutable: Boolean = false
}

final case class PackageSignature(
                                   id: TypeIdentifier,
                                   importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                   importedDevices: mutable.LinkedHashSet[Device],
                                   functions: Map[FunOrVarId, FunctionSignature]
                                 ) extends ModuleOrPackageSignature {
  override def paramImports: mutable.LinkedHashMap[FunOrVarId, Type] = mutable.LinkedHashMap.empty
}

final case class StructSignature(
                                  id: TypeIdentifier,
                                  fields: mutable.LinkedHashMap[FunOrVarId, FieldInfo],
                                  directSupertypes: Seq[TypeIdentifier],
                                  isInterface: Boolean
                                ) extends StructOrModuleSignature {
  override def isModuleOrPackage: Boolean = false

  override def functions: Map[FunOrVarId, FunctionSignature] = Map.empty

  override def constructorSig: FunctionSignature =
    FunctionSignature(ConstructorFunId, fields.map(_._2.tpe).toList, VoidType)

  override def isShallowMutable: Boolean = fields.exists(_._2.isReassignable)
}

final case class DeviceSignature(
                                  id: TypeIdentifier,
                                  functions: Map[FunOrVarId, FunctionSignature]
                                ) extends TypeSignature {

  override def isInterface: Boolean = false

  override def isModuleOrPackage: Boolean = false
}

object StructSignature {
  final case class FieldInfo(tpe: Type, isReassignable: Boolean)
}
