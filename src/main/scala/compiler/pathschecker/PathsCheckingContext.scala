package compiler.pathschecker

import identifiers.FunOrVarId

import scala.collection.mutable

final case class PathsCheckingContext private(localsAndConsts: mutable.Set[FunOrVarId]) {
  
  def copyForNarrowedScope(): PathsCheckingContext =
    copy(mutable.Set.from(localsAndConsts))
  
  def saveLocal(id: FunOrVarId): Unit = {
    localsAndConsts.addOne(id)
  }
  
  def unknownVarsRemoved(state: State): State = State(
    state.alwaysTerminated,
    state.locals.filter((id, _) => localsAndConsts.contains(id))
  )
  
}

object PathsCheckingContext {
  def empty: PathsCheckingContext = PathsCheckingContext(mutable.Set.empty)
}
