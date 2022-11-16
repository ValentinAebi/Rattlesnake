package compiler.ctxcreator

import compiler.Errors.ErrorReporter
import compiler.irs.Asts.{FunDef, Source, StructDef}
import compiler.{AnalysisContext, CompilerStep}

/**
 * Compiler pass to generate an [[AnalysisContext]]
 */
final class ContextCreator(errorReporter: ErrorReporter) extends CompilerStep[List[Source], (List[Source], AnalysisContext)] {

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
      }
    }
    ctxBuilder.build()
  }

}
