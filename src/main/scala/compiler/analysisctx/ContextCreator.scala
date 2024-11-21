package compiler.analysisctx

import compiler.reporting.Errors.ErrorReporter
import compiler.irs.Asts.*
import compiler.pipeline.CompilerStep

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
          case moduleDef: ModuleDef =>
            ctxBuilder.addModule(moduleDef)
          case packageDef: PackageDef =>
            ctxBuilder.addPackage(packageDef)
          case structDef: StructDef =>
            ctxBuilder.addStruct(structDef)
          case constDef: ConstDef =>
            ctxBuilder.addConstant(constDef)
      }
    }
    ctxBuilder.build()
  }

}
