package compiler.capturechecker

import identifiers.FunOrVarId
import lang.{ProgramValue, CapturableValue}

import scala.collection.mutable

final class FieldsTracker {
  private val fields: mutable.Map[CapturableValue, mutable.Map[FunOrVarId, CapturableValue]] = mutable.Map.empty

  def save(owner: CapturableValue, fld: FunOrVarId, value: CapturableValue): Unit = {
    fields.getOrElseUpdate(owner, mutable.Map.empty)
      .put(fld, value)
  }

  def apply(owner: CapturableValue, fld: FunOrVarId): CapturableValue = {
    fields.getOrElseUpdate(owner, mutable.Map.empty)
      .getOrElseUpdate(fld, new ProgramValue(""))
  }
  
}
