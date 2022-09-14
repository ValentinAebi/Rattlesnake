package lang

import lang.Types.Type

import java.util
import scala.collection.mutable

final case class FunctionSignature(name: String, argTypes: List[Type], retType: Type)

final case class StructSignature(name: String, fields: mutable.LinkedHashMap[String, Type])
