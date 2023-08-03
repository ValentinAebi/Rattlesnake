package compiler.typechecker

import compiler.{AnalysisContext, Errors, Position}
import compiler.Errors.{ErrorReporter, Warning, errorsExitCode}
import identifiers.FunOrVarId
import lang.Types.PrimitiveType.{NothingType, VoidType}
import lang.Types.Type
import compiler.CompilationStep.TypeChecking
import compiler.irs.Asts.{Expr, Indexing, Select, VariableRef}
import compiler.typechecker.TypeCheckingContext.LocalInfo
import lang.Keyword

import scala.collection.mutable

/**
 * Mutabble context for type checking
 */
final case class TypeCheckingContext(
                                      private val analysisContext: AnalysisContext,
                                      private val locals: mutable.Map[FunOrVarId, LocalInfo] = mutable.Map.empty
                                    ) {
  // Locals that have been created by this context and not obtained via copied
  private val ownedLocals: mutable.Set[FunOrVarId] = mutable.Set.empty

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
   *
   * @param name                  name of the local
   * @param tpe                   type of the local
   * @param isReassignable        whether it is allowed or not to assign a new value to this local (`val` vs `var`)
   * @param duplicateVarCallback  to be called if the name is already used by another local
   * @param forbiddenTypeCallback to be called if the local has a type that is not acceptable for a local
   */
  def addLocal(
                name: FunOrVarId,
                tpe: Type,
                defPos: Option[Position],
                isReassignable: Boolean,
                duplicateVarCallback: () => Unit,
                forbiddenTypeCallback: () => Unit
              ): Unit = {
    if (tpe == NothingType || tpe == VoidType) {
      forbiddenTypeCallback()
    } else if (locals.contains(name)) {
      duplicateVarCallback()
    } else {
      locals.put(name, LocalInfo(name, tpe, isReassignable, defPos))
      ownedLocals.addOne(name)
    }
  }

  def get(name: FunOrVarId): Option[LocalInfo] = {
    locals.get(name).orElse(
      analysisContext.constants
        .get(name)
        .map(tpe => LocalInfo(name, tpe, isReassignable = false, defPos = None)) // position not used if constant
    )
  }

  def localIsQueried(localId: FunOrVarId): Unit = {
    locals.get(localId).foreach { l =>
      l.queried = true
    }
  }

  def varIsAssigned(varId: FunOrVarId): Unit = {
    locals.get(varId).foreach { l =>
      l.reassigned = true
    }
  }

  def mutIsUsed(localId: FunOrVarId): Unit = {
    locals.get(localId).foreach { l =>
      l.mutUsed = true
    }
  }

  def mutIsUsed(assigLhs: Expr): Unit = {
    assigLhs match
      case Indexing(VariableRef(name), _) => mutIsUsed(name)
      case Select(VariableRef(name), _) => mutIsUsed(name)
      case _ => ()
  }

  def writeLocalsRelatedWarnings(errorReporter: ErrorReporter): Unit = {
    for (_, local@LocalInfo(name, tpe, isReassignable, defPos)) <- locals if ownedLocals.contains(name) do {
      if (!local.queried){
        errorReporter.push(Warning(TypeChecking, s"unused local: '$name' is never queried", defPos))
      } else if (isReassignable && !local.reassigned){
        errorReporter.push(Warning(TypeChecking, s"value declared as variable: '$name' could be a ${Keyword.Val}", defPos))
      }
      if (tpe.isModifiable && !local.mutUsed){
        errorReporter.push(Warning(TypeChecking, s"unused modification privilege: '$name' could have type '${tpe.unmodifiable}'", defPos))
      }
    }
  }

  export analysisContext.*

}

object TypeCheckingContext {

  final case class LocalInfo(name: FunOrVarId, tpe: Type, isReassignable: Boolean, defPos: Option[Position]){
    private[TypeCheckingContext] var queried = false
    private[TypeCheckingContext] var reassigned = false
    private[TypeCheckingContext] var mutUsed = false
  }

}
