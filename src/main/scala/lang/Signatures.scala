package lang

import identifiers.*
import lang.StructSignature.FieldInfo
import lang.Types.Type

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
}

sealed trait ModuleOrPackageSignature extends TypeSignature {
  def functions: Map[FunOrVarId, FunctionSignature]
  override def isInterface: Boolean = false
  override def isModuleOrPackage: Boolean = true
}

final case class ModuleSignature(
                                  name: TypeIdentifier,
                                  paramTypes: mutable.LinkedHashMap[FunOrVarId, Type],
                                  functions: Map[FunOrVarId, FunctionSignature]
                                ) extends ModuleOrPackageSignature

final case class PackageSignature(
                                   name: TypeIdentifier,
                                   functions: Map[FunOrVarId, FunctionSignature]
                                 ) extends ModuleOrPackageSignature

final case class StructSignature(
                                  name: TypeIdentifier,
                                  fields: mutable.LinkedHashMap[FunOrVarId, FieldInfo],
                                  directSupertypes: Seq[TypeIdentifier],
                                  isInterface: Boolean
                                ) extends TypeSignature {
  override def isModuleOrPackage: Boolean = false
}

object StructSignature {
  final case class FieldInfo(tpe: Type, isReassignable: Boolean)
}
