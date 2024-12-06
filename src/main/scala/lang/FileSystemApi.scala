package lang

import identifiers.FunOrVarId

object FileSystemApi extends Device.DeviceApi {

  override def functions: Map[FunOrVarId, FunctionSignature] = Map.empty
  
}
