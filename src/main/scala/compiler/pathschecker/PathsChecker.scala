package compiler.pathschecker

import compiler.analysisctx.AnalysisContext
import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.pipeline.CompilationStep.PathsChecking
import compiler.pipeline.CompilerStep
import compiler.reporting.Errors.{Err, ErrorReporter, Warning}
import lang.Types.PrimitiveTypeShape.{NothingType, VoidType}


final class PathsChecker(er: ErrorReporter) extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, _) = input
    for (src <- sources; df <- src.defs){
      df match
        case moduleOrPackageDefTree: ModuleOrPackageDefTree =>
          for (fun <- moduleOrPackageDefTree.functions){
            checkFunction(fun)
          }
        case StructDef(structName, fields, directSupertypes, isInterface) => ()
        case ConstDef(constName, tpeOpt, value) => ()
    }
    er.displayAndTerminateIfErrors()
    input
  }

  private def checkFunction(function: FunDef): Unit = {
    val endState = analyzeStat(State.initial, function.body)(using PathsCheckingContext.empty)
    val retType = function.getSignatureOpt.get.retType
    if (!endState.alwaysTerminated && retType != VoidType) {
      er.push(Err(PathsChecking, "missing return in non-Void function", function.getPosition))
    }
    if (retType == NothingType && !endState.alwaysTerminated) {
      er.push(Err(PathsChecking,
        s"cannot prove that function '${function.funName}' with return type '$NothingType' cannot return",
        function.getPosition
      ))
    }
  }

  private def analyzeStat(inState: State, statement: Statement)(using ctx: PathsCheckingContext): State = statement match {
    case expr: Expr => analyzeExpr(inState, expr)
    case Block(stats) =>
      val newCtx = ctx.copyForNarrowedScope()
      var state = inState
      var alreadyReportedDeadCode = false
      for (stat <- stats) {
        alreadyReportedDeadCode |= state.checkIsReachable(er, stat.getPosition)
        state = analyzeStat(state, stat)(using newCtx)
      }
      ctx.unknownVarsRemoved(state)
    case LocalDef(localName, optTypeAnnot, rhsOpt, isReassignable) =>
      val s = rhsOpt.map(analyzeExpr(inState, _)).getOrElse(inState)
      ctx.saveLocal(localName)
      s.newLocalSaved(localName, rhsOpt.isDefined)
    case VarAssig(lhs, rhs) =>
      val s1 = stateAfterEvaluatingAssignmentTarget(inState, lhs)
      val s2 = analyzeExpr(s1, rhs)
      markedAssignedIfVariable(lhs, s2)
    case VarModif(lhs, rhs, op) =>
      val s = analyzeExpressions(inState, lhs, rhs)
      markedAssignedIfVariable(lhs, s)
    case IfThenElse(cond, thenBr, elseBrOpt) =>
      val stateAfterCond = analyzeExpr(inState, cond)
      val stateAfterThen = analyzeStat(stateAfterCond, thenBr)
      val stateAfterElse = elseBrOpt.map(analyzeStat(stateAfterCond, _)).getOrElse(stateAfterCond)
      stateAfterThen.joined(stateAfterElse)
    case loop@WhileLoop(cond, body) =>
      val stateAfterInitCondEval = analyzeExpr(inState, cond)
      val stateAfterBody = analyzeStat(stateAfterInitCondEval, body)
      if (stateAfterBody.alwaysTerminated){
        er.push(Warning(PathsChecking, "while does not loop, it should probably be an if", loop.getPosition))
      }
      val stateAfterLoop = stateAfterInitCondEval.joined(stateAfterBody)
      if isTrueLiteral(cond) then stateAfterLoop.terminated() else stateAfterLoop
    case loop@ForLoop(initStats, cond, stepStats, body) =>
      val stateAfterInitStats = analyzeStatements(inState, initStats)
      val stateAfterInitCondEval = analyzeStat(stateAfterInitStats, cond)
      val stateAfterBody = analyzeStat(stateAfterInitCondEval, body)
      if (stateAfterBody.alwaysTerminated){
        er.push(Warning(PathsChecking, "for does not loop, it should probably be an if", loop.getPosition))
      }
      val stateAfterLoop = stateAfterInitCondEval.joined(stateAfterBody)
      if isTrueLiteral(cond) then stateAfterLoop.terminated() else stateAfterLoop
    case ReturnStat(optVal) =>
      optVal.map(analyzeExpr(inState, _))
        .getOrElse(inState)
        .terminated()
    case PanicStat(msg) =>
      analyzeExpr(inState, msg).terminated()
    case RestrictedStat(ExplicitCaptureSetTree(capturedExpressions), body) =>
      val s = analyzeExpressions(inState, capturedExpressions)
      analyzeStat(s, body)
    case EnclosedStat(ExplicitCaptureSetTree(capturedExpressions), body) =>
      val s = analyzeExpressions(inState, capturedExpressions)
      analyzeStat(s, body)
  }

  private def analyzeExpr(inState: State, expr: Expr)(using ctx: PathsCheckingContext): State = expr match {
    case literal: Literal => inState
    case RegionCreation() => inState
    case varRef: VariableRef =>
      inState.checkIsInitialized(varRef, er)
      inState
    case MeRef() => inState
    case PackageRef(pkgName) => inState
    case DeviceRef(device) => inState
    case call@Call(receiverOpt, function, args) =>
      val preCallState = analyzeExpressions(inState, receiverOpt ++ args)
      if call.getSignatureOpt.get.retType == NothingType
      then preCallState.terminated()
      else preCallState
    case Indexing(indexed, arg) =>
      analyzeExpressions(inState, indexed, arg)
    case ArrayInit(region, elemType, size) =>
      analyzeExpressions(inState, region, size)
    case FilledArrayInit(regionOpt, arrayElems) =>
      analyzeExpressions(inState, regionOpt ++ arrayElems)
    case StructOrModuleInstantiation(regionOpt, typeId, args) =>
      analyzeExpressions(inState, regionOpt ++ args)
    case UnaryOp(operator, operand) =>
      analyzeExpr(inState, operand)
    case BinaryOp(lhs, operator, rhs) =>
      analyzeExpressions(inState, lhs, rhs)
    case Select(lhs, selected) =>
      analyzeExpr(inState, lhs)
    case Ternary(cond, thenBr, elseBr) =>
      val stateAfterCond = analyzeExpr(inState, cond)
      val stateAfterThen = analyzeExpr(stateAfterCond, thenBr)
      val stateAfterElse = analyzeExpr(stateAfterCond, elseBr)
      stateAfterThen.joined(stateAfterElse)
    case Cast(castedExpr, tpe) =>
      analyzeExpr(inState, castedExpr)
    case TypeTest(testedExpr, tpe) =>
      analyzeExpr(inState, testedExpr)
    case Sequence(stats, expr) =>
      val s = analyzeStatements(inState, stats)
      analyzeExpr(s, expr)
  }

  private def analyzeStatements(inState: State, statSeq: Iterable[Statement])(using PathsCheckingContext): State =
    statSeq.foldLeft(inState)(analyzeStat)

  private def analyzeExpressions(inState: State, exprSeq: Iterable[Expr])(using PathsCheckingContext): State =
    exprSeq.foldLeft(inState)(analyzeExpr)

  private def analyzeExpressions(inState: State, exprSeq: Expr*)(using PathsCheckingContext): State =
    exprSeq.foldLeft(inState)(analyzeExpr)

  private def stateAfterEvaluatingAssignmentTarget(inState: State, target: Expr)(using PathsCheckingContext): State = {
    target match {
      case VariableRef(name) => inState
      case Select(lhs, selected) => analyzeExpr(inState, lhs)
      case Indexing(indexed, arg) => analyzeExpressions(inState, indexed, arg)
      case lhs => throw new AssertionError(s"unexpected ${lhs.getClass.getSimpleName} during $PathsChecking")
    }
  }

  private def markedAssignedIfVariable(assignmentTarget: Expr, state: State): State = {
    assignmentTarget match {
      case VariableRef(name) => state.assignmentSaved(name)
      case _ => state
    }
  }

  private def isTrueLiteral(expr: Expr): Boolean = {
    expr match
      case BoolLit(true) => true
      case _ => false
  }

}
