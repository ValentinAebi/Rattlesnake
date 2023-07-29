package compiler.typechecker

import compiler.AnalysisContext
import identifiers.FunOrVarId
import lang.Types.PrimitiveType.{NothingType, VoidType}
import lang.Types.Type

import scala.collection.mutable

/**
 * Mutabble context for type checking
 */
final case class TypeCheckingContext(
                                      analysisContext: AnalysisContext,
                                      locals: mutable.Map[FunOrVarId, (Type, Boolean)] = mutable.Map.empty
                                    ) {

  /**
   * @return a copy of this with empty locals map
   */
  def copyWithoutLocals: TypeCheckingContext = {
    copy(locals = mutable.Map.empty)
  }

  /**
   * @return a (deep) copy of this
   */
  def copied: TypeCheckingContext = {
    copy(locals = mutable.Map.from(locals))
  }

  /**
   * Register a new local
   * @param name name of the local
   * @param tpe type of the local
   * @param isReassignable whether it is allowed or not to assign a new value to this local (`val` vs `var`)
   * @param duplicateVarCallback to be called if the name is already used by another local
   * @param forbiddenTypeCallback to be called if the local has a type that is not acceptable for a local
   */
  def addLocal(name: FunOrVarId, tpe: Type, isReassignable: Boolean, duplicateVarCallback: () => Unit, forbiddenTypeCallback: () => Unit): Unit = {
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
