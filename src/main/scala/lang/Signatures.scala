package lang

import identifiers.*
import lang.Captures.*
import lang.StructSignature.FieldInfo
import lang.Types.{NamedType, Type}

import java.util
import scala.collection.mutable

final case class FunctionSignature(
                                    name: FunOrVarId,
                                    argTypes: List[Type],
                                    retType: Type
                                  )

sealed trait TypeSignature {
  def isInterface: Boolean

  def isModuleOrPackage: Boolean

  def functions: Map[FunOrVarId, FunctionSignature]
}

sealed trait ModuleOrPackageSignature extends TypeSignature {
  def paramImports: mutable.LinkedHashMap[FunOrVarId, TypeIdentifier]

  def importedPackages: mutable.LinkedHashSet[TypeIdentifier]

  def importedDevices: mutable.LinkedHashSet[Device]

  override def isInterface: Boolean = false

  override def isModuleOrPackage: Boolean = true
}

final case class ModuleSignature(
                                  name: TypeIdentifier,
                                  paramImports: mutable.LinkedHashMap[FunOrVarId, Type],
                                  importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                  importedDevices: mutable.LinkedHashSet[Device],
                                  functions: Map[FunOrVarId, FunctionSignature]
                                ) extends ModuleOrPackageSignature {
  def rawCaptureSet: CaptureSet = CaptureSet(
    paramImports.map((id, tpe) => SelectPath(MePath, id)).toSet ++
      importedPackages.map(pkgName => PackagePath(pkgName)).toSet ++
      importedDevices.map(DevicePath(_)).toSet
  )
}

final case class PackageSignature(
                                   name: TypeIdentifier,
                                   importedPackages: mutable.LinkedHashSet[TypeIdentifier],
                                   importedDevices: mutable.LinkedHashSet[Device],
                                   functions: Map[FunOrVarId, FunctionSignature]
                                 ) extends ModuleOrPackageSignature {
  override def paramImports: mutable.LinkedHashMap[FunOrVarId, TypeIdentifier] = mutable.LinkedHashMap.empty

  def captureSet: CaptureSet = CaptureSet(
    importedPackages.map(pkgName => PackagePath(pkgName)).toSet ++
      importedDevices.map(DevicePath(_)).toSet
  )
}

final case class StructSignature(
                                  name: TypeIdentifier,
                                  fields: mutable.LinkedHashMap[FunOrVarId, FieldInfo],
                                  directSupertypes: Seq[TypeIdentifier],
                                  isInterface: Boolean
                                ) extends TypeSignature {
  override def isModuleOrPackage: Boolean = false

  override def functions: Map[FunOrVarId, FunctionSignature] = Map.empty
}

final case class DeviceSignature(
                                  typeId: TypeIdentifier,
                                  functions: Map[FunOrVarId, FunctionSignature]
                                ) extends TypeSignature {
  override def isInterface: Boolean = false

  override def isModuleOrPackage: Boolean = false
}

object StructSignature {
  final case class FieldInfo(tpe: Type, isReassignable: Boolean)
}
