package compiler.typechecker

import compiler.AnalysisContext
import lang.Types.Type
import lang.Types.PrimitiveType.{VoidType, NothingType}

import scala.collection.mutable

final case class TypeCheckingContext(analysisContext: AnalysisContext, locals: mutable.Map[String, (Type, Boolean)] = mutable.Map.empty) {

  def copyWithoutLocals: TypeCheckingContext = {
    copy(locals = mutable.Map.empty)
  }

  def copied: TypeCheckingContext = {
    copy(locals = mutable.Map.from(locals))
  }

  def addLocal(name: String, tpe: Type, isReassignable: Boolean, duplicateVarCallback: () => Unit, forbiddenTypeCallback: () => Unit): Unit = {
    if (tpe == NothingType || tpe == VoidType) {
      forbiddenTypeCallback()
    } else if (locals.contains(name)) {
      duplicateVarCallback()
    } else {
      locals.put(name, (tpe, isReassignable))
    }
  }

  export analysisContext.*

}
