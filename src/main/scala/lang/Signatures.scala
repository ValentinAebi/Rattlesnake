package lang

import identifiers.*
import lang.StructSignature.FieldInfo
import lang.Types.Type

import java.util
import scala.collection.mutable

final case class FunctionSignature(name: FunOrVarId, argTypes: List[Type], retType: Type)

final case class StructSignature(name: TypeIdentifier, fields: mutable.LinkedHashMap[FunOrVarId, FieldInfo])

object StructSignature {
  final case class FieldInfo(tpe: Type, isReassignable: Boolean)
}
