package lang

import lang.Types.Type

final case class FunctionSignature(name: String, argTypes: List[Type], retType: Type)

final case class StructSignature(name: String, fields: Map[String, Type])
