package compiler.ctxcreator

import compiler.Errors.ErrorReporter
import compiler.irs.Asts.{ConstDef, FunDef, Source, StructDef, TestDef}
import compiler.{AnalysisContext, CompilerStep, FunctionsToInject}

/**
 * Compiler pass to generate an [[AnalysisContext]]
 */
final class ContextCreator(errorReporter: ErrorReporter, functionsToInject: List[FunDef]) extends CompilerStep[List[Source], (List[Source], AnalysisContext)] {

  override def apply(sources: List[Source]): (List[Source], AnalysisContext) = {
    val ctx = buildContext(sources)
    errorReporter.displayAndTerminateIfErrors()
    (sources, ctx)
  }

  private def buildContext(sources: List[Source]): AnalysisContext = {
    val ctxBuilder = new AnalysisContext.Builder(errorReporter)
    for src <- sources do {
      for df <- src.defs do {
        df match
          case funDef: FunDef =>
            ctxBuilder.addFunction(funDef)
          case structDef: StructDef =>
            ctxBuilder.addStruct(structDef)
          case testDef: TestDef =>
            ctxBuilder.addTest(testDef)
          case constDef: ConstDef =>
            ctxBuilder.addConstant(constDef)
      }
    }
    for df <- functionsToInject do {
      ctxBuilder.addFunction(df)
    }
    ctxBuilder.build()
  }

}
