package compiler.capturechecker

import compiler.irs.Asts.Param
import lang.{ProgramValue, CapturableValue}

import scala.collection.mutable

final case class State(registers: mutable.Map[Register, CapturableValue]) {
  
  def deepCopy(): State = State(mutable.Map.from(registers))
  
  export registers.{apply, update}
  
}

object State {
  
  def fromFunParams(params: List[Param]): State = {
    val variables = mutable.Map.empty[Register, CapturableValue]
    for (param <- params){
      param.paramNameOpt.foreach { paramName =>
        variables.put(VarRegister(paramName), new ProgramValue(paramName.stringId))
      }
    }
    State(variables)
  }
  
}
