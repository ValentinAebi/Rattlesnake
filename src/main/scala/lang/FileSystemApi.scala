package lang

import identifiers.{FunOrVarId, NormalFunOrVarId}
import lang.Types.PrimitiveTypeShape.{BoolType, IntType, StringType, VoidType}

object FileSystemApi extends Device.DeviceApi {

  val openR: NormalFunOrVarId = NormalFunOrVarId("openR")
  val openW: NormalFunOrVarId = NormalFunOrVarId("openW")
  val write: NormalFunOrVarId = NormalFunOrVarId("write")
  val read: NormalFunOrVarId = NormalFunOrVarId("read")
  val close: NormalFunOrVarId = NormalFunOrVarId("close")
  val createDir: NormalFunOrVarId = NormalFunOrVarId("createDir")
  val delete: NormalFunOrVarId = NormalFunOrVarId("delete")


  override def functions: Map[FunOrVarId, FunctionSignature] = Map(
    FunctionSignature(openR, List(None -> StringType), IntType).keyed,
    FunctionSignature(openW, List(None -> StringType), IntType).keyed,
    FunctionSignature(write, List(None -> IntType, None -> StringType), VoidType).keyed,
    FunctionSignature(read, List(None -> IntType), IntType).keyed,
    FunctionSignature(close, List(None -> IntType), VoidType).keyed,
    FunctionSignature(createDir, List(None -> StringType), BoolType).keyed,
    FunctionSignature(delete, List(None -> StringType), BoolType).keyed
  )

  extension (sig: FunctionSignature) private def keyed: (FunOrVarId, FunctionSignature) = {
    sig.name -> sig
  }
  
}
