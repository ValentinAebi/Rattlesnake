package lang

import lang.Types.ArrayType
import lang.Types.PrimitiveType.*

object BuiltInFunctions {

  val builtInFunctions: Map[String, FunctionSignature] = Map(
    FunctionSignature("print", List(StringType), VoidType).keyed
  )

  extension(sig: FunctionSignature) private def keyed: (String, FunctionSignature) = {
    sig.name -> sig
  }

}
