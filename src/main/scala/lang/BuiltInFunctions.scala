package lang

import lang.Types.ArrayType
import lang.Types.PrimitiveType.*

object BuiltInFunctions {
  
  val print: String = "print"
  val intToString: String = "intToString"
  val doubleToString: String = "doubleToString"
  val charToString: String = "charToString"
  val boolToString: String = "boolToString"
  val toCharArray: String = "toCharArray"

  val builtInFunctions: Map[String, FunctionSignature] = Map(
    FunctionSignature(print, List(StringType), VoidType).keyed,
    FunctionSignature(intToString, List(IntType), StringType).keyed,
    FunctionSignature(doubleToString, List(DoubleType), StringType).keyed,
    FunctionSignature(charToString, List(CharType), StringType).keyed,
    FunctionSignature(boolToString, List(BoolType), StringType).keyed,
    FunctionSignature(toCharArray, List(StringType), ArrayType(CharType)).keyed
  )

  extension(sig: FunctionSignature) private def keyed: (String, FunctionSignature) = {
    sig.name -> sig
  }

}
