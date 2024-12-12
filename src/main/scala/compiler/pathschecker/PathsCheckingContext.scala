package compiler.pathschecker

import identifiers.FunOrVarId

import scala.collection.mutable

/**
 * @param localsAndConsts localId -> isReassignable
 */
final class PathsCheckingContext private(localsAndConsts: mutable.Map[FunOrVarId, Boolean]) {
  
  def copyForNarrowedScope(): PathsCheckingContext =
    PathsCheckingContext(mutable.Map.from(localsAndConsts))
  
  def saveLocal(id: FunOrVarId, isReassignable: Boolean): Unit = {
    localsAndConsts.put(id, isReassignable)
  }
  
  def isReassignable(id: FunOrVarId): Boolean = localsAndConsts.apply(id)
  
  def unknownVarsRemoved(state: State): State = State(
    state.alwaysTerminated,
    state.locals.filter((id, _) => localsAndConsts.contains(id))
  )
  
}

object PathsCheckingContext {
  def empty: PathsCheckingContext = PathsCheckingContext(mutable.Map.empty)
}
