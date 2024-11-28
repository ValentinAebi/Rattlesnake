package lang

import identifiers.*
import lang.Types.ArrayTypeShape
import lang.Types.PrimitiveTypeShape.*

object Intrinsics {
  
  val print: NormalFunOrVarId = NormalFunOrVarId("print")
  val intToString: NormalFunOrVarId = NormalFunOrVarId("intToString")
  val doubleToString: NormalFunOrVarId = NormalFunOrVarId("doubleToString")
  val charToString: NormalFunOrVarId = NormalFunOrVarId("charToString")
  val boolToString: NormalFunOrVarId = NormalFunOrVarId("boolToString")
  val toCharArray: NormalFunOrVarId = NormalFunOrVarId("toCharArray")

  val intrinsics: Map[FunOrVarId, FunctionSignature] = Map(
    FunctionSignature(print, List(None -> StringType), VoidType).keyed,
    FunctionSignature(intToString, List(None -> IntType), StringType).keyed,
    FunctionSignature(doubleToString, List(None -> DoubleType), StringType).keyed,
    FunctionSignature(charToString, List(None -> CharType), StringType).keyed,
    FunctionSignature(boolToString, List(None -> BoolType), StringType).keyed,
    FunctionSignature(toCharArray, List(None -> StringType), ArrayTypeShape(CharType, modifiable = true)).keyed
  )

  extension(sig: FunctionSignature) private def keyed: (FunOrVarId, FunctionSignature) = {
    sig.name -> sig
  }

}
