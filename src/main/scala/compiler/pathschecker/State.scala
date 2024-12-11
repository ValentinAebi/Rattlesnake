package compiler.pathschecker

import compiler.irs.Asts.VariableRef
import compiler.pathschecker.InitializationStatus.{Initialized, Uninitialized}
import compiler.pipeline.CompilationStep.PathsChecking
import compiler.reporting.Errors.{Err, ErrorReporter}
import compiler.reporting.Position
import identifiers.FunOrVarId

final case class State(alwaysTerminated: Boolean, locals: Map[FunOrVarId, InitializationStatus]) {

  def checkIsInitialized(varRef: VariableRef, er: ErrorReporter): Unit = {
    locals.get(varRef.name).foreach {
      case InitializationStatus.Initialized => ()
      case InitializationStatus.Uninitialized =>
        er.push(Err(PathsChecking, s"variable '${varRef.name}' has not been initialized", varRef.getPosition))
      case InitializationStatus.PossiblyUninitialized =>
        er.push(Err(PathsChecking, s"cannot prove that variable '${varRef.name}' has been initialized", varRef.getPosition))
    }
  }

  /**
   * @return true iff reachable
   */
  def checkIsReachable(er: ErrorReporter, posOpt: Option[Position]): Boolean = {
    if (alwaysTerminated){
      er.push(Err(PathsChecking, "unreachable code", posOpt))
    }
    !alwaysTerminated
  }

  def terminated(): State = copy(alwaysTerminated = true)

  def newLocalSaved(localId: FunOrVarId, initialized: Boolean): State = {
    val initializationStatus = if initialized then Initialized else Uninitialized
    copy(locals = locals.updated(localId, initializationStatus))
  }

  def assignmentSaved(localId: FunOrVarId): State = {
    copy(locals = locals.updated(localId, Initialized))
  }

  def joined(that: State): State = State(
    this.alwaysTerminated && that.alwaysTerminated,
    (for (localId <- (this.locals.keys ++ that.locals.keys))
      yield localId -> joined(
        this.locals.get(localId),
        that.locals.get(localId)
      )).toMap
  )

  private def joined(lStatusOpt: Option[InitializationStatus], rStatusOpt: Option[InitializationStatus]): InitializationStatus = {
    (lStatusOpt, rStatusOpt) match {
      case (Some(s), None) => s
      case (None, Some(s)) => s
      case (Some(s1), Some(s2)) => s1.joined(s2)
      case _ => throw new AssertionError("should not happen")
    }
  }

}

object State {

  def initial: State = State(false, Map.empty)

}
