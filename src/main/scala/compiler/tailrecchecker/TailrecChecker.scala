package compiler.tailrecchecker

import compiler.CompilationStep.TailrecChecking
import compiler.Errors.ErrorReporter
import compiler.irs.Asts
import compiler.{AnalysisContext, CompilerStep, Errors}
import compiler.irs.Asts.*

final class TailrecChecker(errorReporter: ErrorReporter)
  extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val sources = input._1
    for (src <- sources){
      check(src, false)
    }
    errorReporter.displayAndTerminateIfErrors()
    input
  }

  private def check(ast: Ast, isTailPosition: Boolean): Unit = {
    ast match {
      case TailCall(name, args) if !isTailPosition =>
        errorReporter.push(Errors.Err(TailrecChecking, s"call to '$name' is not in tail position", ast.getPosition))
        args.foreach(check(_, false))
      case Ternary(cond, thenBr, elseBr) =>
        check(cond, false)
        check(thenBr, isTailPosition)
        check(elseBr, isTailPosition)
      case ReturnStat(Some(expr)) =>
        check(expr, true)
      case _ =>
        ast.children.foreach(check(_, false))
    }
  }

}
