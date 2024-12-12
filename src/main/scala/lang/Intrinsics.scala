package lang

import identifiers.*
import lang.LanguageMode.OcapEnabled
import lang.Types.PrimitiveTypeShape.*

object Intrinsics {
  
  val print: NormalFunOrVarId = NormalFunOrVarId("print")
  val intToString: NormalFunOrVarId = NormalFunOrVarId("intToString")
  val doubleToString: NormalFunOrVarId = NormalFunOrVarId("doubleToString")
  val charToString: NormalFunOrVarId = NormalFunOrVarId("charToString")
  val boolToString: NormalFunOrVarId = NormalFunOrVarId("boolToString")

  val intrinsics: Map[FunOrVarId, FunctionSignature] = Map(
    FunctionSignature(print, List(None -> StringType), VoidType, OcapEnabled).keyed,
    FunctionSignature(intToString, List(None -> IntType), StringType, OcapEnabled).keyed,
    FunctionSignature(doubleToString, List(None -> DoubleType), StringType, OcapEnabled).keyed,
    FunctionSignature(charToString, List(None -> CharType), StringType, OcapEnabled).keyed,
    FunctionSignature(boolToString, List(None -> BoolType), StringType, OcapEnabled).keyed
  )

  extension(sig: FunctionSignature) private def keyed: (FunOrVarId, FunctionSignature) = {
    sig.name -> sig
  }

}
