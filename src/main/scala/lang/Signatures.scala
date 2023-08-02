package lang

import identifiers.*
import lang.Types.Type

import java.util
import scala.collection.mutable

final case class FunctionSignature(name: FunOrVarId, argTypes: List[Type], retType: Type)

// TODO optional const/mut (?) for fields
final case class StructSignature(name: TypeIdentifier, fields: mutable.LinkedHashMap[FunOrVarId, Type])
