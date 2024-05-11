package compiler.typechecker

import compiler.{AnalysisContext, Errors, Position}
import compiler.Errors.{ErrorReporter, Warning, errorsExitCode}
import identifiers.FunOrVarId
import lang.Types.PrimitiveType.{NothingType, VoidType}
import lang.Types.Type
import compiler.CompilationStep.TypeChecking
import compiler.irs.Asts.{Expr, Indexing, Select, VariableRef}
import compiler.typechecker.TypeCheckingContext.{LocalInfo, LocalUsesCollector}
import lang.Keyword

import scala.collection.mutable

/**
 * Mutabble context for type checking
 */
final case class TypeCheckingContext(
                                      private val analysisContext: AnalysisContext,
                                      private val locals: mutable.Map[FunOrVarId, LocalInfo] = mutable.Map.empty,
                                      expectedRetType: Type
                                    ) {
  // Locals that have been created by this context (i.e. not obtained via copied)
  private val ownedLocals: mutable.Set[FunOrVarId] = mutable.Set.empty

  /**
   * @return a copy of this with empty locals map
   */
  def copyWithoutLocals(expectedRetType: Type): TypeCheckingContext = {
    copy(locals = mutable.Map.empty, expectedRetType = expectedRetType)
  }

  /**
   * @return a (deep) copy of this
   */
  def copied: TypeCheckingContext = {
    copy(locals = mutable.Map.from(locals))
  }

  // TODO keep smartcasts on vars, until reassignment (currently practically unusable on loops)
  def copyWithSmartCasts(smartCasts: Map[FunOrVarId, Type]): TypeCheckingContext = {
    copy(locals = locals.map {
      case (id, info) => id -> info.copy(tpe = smartCasts.getOrElse(id, info.tpe))
    })
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
                declHasTypeAnnot: Boolean,
                duplicateVarCallback: () => Unit,
                forbiddenTypeCallback: () => Unit
              ): Unit = {
    if (tpe == NothingType || tpe == VoidType) {
      forbiddenTypeCallback()
    } else if (locals.contains(name)) {
      duplicateVarCallback()
    } else {
      locals.put(name, LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot))
      ownedLocals.addOne(name)
    }
  }

  def getLocalOnly(name: FunOrVarId): Option[LocalInfo] = locals.get(name)

  def getLocalOrConst(name: FunOrVarId): Option[LocalInfo] = {
    getLocalOnly(name).orElse(
      analysisContext.constants
        .get(name)
        // defPos and declHasTypeAnnot are never used for constants, as long as constants can only be of primitive types
        .map(tpe => LocalInfo(name, tpe, isReassignable = false, defPos = None, declHasTypeAnnot = false))
    )
  }

  def localIsQueried(localId: FunOrVarId): Unit = {
    locals.get(localId).foreach { l =>
      l.usesCollector.queried = true
    }
  }

  def varIsAssigned(varId: FunOrVarId): Unit = {
    locals.get(varId).foreach { l =>
      l.usesCollector.reassigned = true
    }
  }

  def mutIsUsed(localId: FunOrVarId): Unit = {
    locals.get(localId).foreach { l =>
      l.usesCollector.mutUsed = true
    }
  }

  def mutIsUsed(assigLhs: Expr): Unit = {
    assigLhs match
      case Indexing(VariableRef(name), _) => mutIsUsed(name)
      case Select(VariableRef(name), _) => mutIsUsed(name)
      case _ => ()
  }

  def writeLocalsRelatedWarnings(errorReporter: ErrorReporter): Unit = {
    for (_, LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, usesCollector)) <- locals if ownedLocals.contains(name) do {
      if (!usesCollector.queried){
        errorReporter.push(Warning(TypeChecking, s"unused local: '$name' is never queried", defPos))
      } else if (isReassignable && !usesCollector.reassigned){
        errorReporter.push(Warning(TypeChecking, s"value declared as variable: '$name' could be a ${Keyword.Val}", defPos))
      }
      if (tpe.maybeModifiable && !usesCollector.mutUsed && declHasTypeAnnot){
        errorReporter.push(Warning(TypeChecking, s"unused modification privilege: '$name' could have type '${tpe.unmodifiable}'", defPos))
      }
    }
  }

  export analysisContext.*

}

object TypeCheckingContext {

  final case class LocalInfo private(
                                      name: FunOrVarId,
                                      tpe: Type,
                                      isReassignable: Boolean,
                                      defPos: Option[Position],
                                      declHasTypeAnnot: Boolean,
                                      usesCollector: LocalUsesCollector
                                    ){
    def copy(
              name: FunOrVarId = name,
              tpe: Type = tpe,
              isReassignable: Boolean = isReassignable,
              defPos: Option[Position] = defPos,
              declHasTypeAnnot: Boolean = declHasTypeAnnot,
              usesCollector: LocalUsesCollector = usesCollector
            ): LocalInfo = {
      LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, usesCollector)
    }
  }

  object LocalInfo {
    def apply(name: FunOrVarId,
              tpe: Type,
              isReassignable: Boolean,
              defPos: Option[Position],
              declHasTypeAnnot: Boolean): LocalInfo = {
      LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, new LocalUsesCollector())
    }
  }

  final class LocalUsesCollector {
    var queried = false
    var reassigned = false
    var mutUsed = false
  }

}
