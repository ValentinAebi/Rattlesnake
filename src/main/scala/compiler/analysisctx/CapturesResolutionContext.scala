package compiler.analysisctx

import compiler.irs.Asts.*
import identifiers.FunOrVarId
import lang.Types.UndefinedTypeShape
import lang.*

import scala.collection.mutable

final class CapturesResolutionContext {
  private val localValues: mutable.Map[FunOrVarId, CapturableValue] = mutable.Map.empty
  private val fields: mutable.Map[(CapturableValue, FunOrVarId), CapturableValue] = mutable.Map.empty

  def addLocal(id: FunOrVarId, value: CapturableValue): Unit = {
    localValues(id) = value
  }
    
  def addField(owner: CapturableValue, fld: FunOrVarId, value: CapturableValue): Unit = {
    fields((owner, fld)) = value
  }

  def resolveCapturedExpr(expr: Expr): CapturableValue = expr match {
    case MeRef() => MeValue()
    case PackageRef(pkgName) => PackageValue(pkgName)
    case DeviceRef(device) => DeviceValue(device)
    case VariableRef(name) => localValues.getOrElse(name, UndefinedValue())
    case Select(lhs, selected) =>
      val lhsVal = resolveCapturedExpr(lhs)
      fields.getOrElse((lhsVal, selected), SelectValue(lhsVal, selected))
    case _ => UndefinedValue()
  }

}
