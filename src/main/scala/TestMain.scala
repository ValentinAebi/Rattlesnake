import compiler.Errors.ErrorReporter
import compiler.backend.Backend
import compiler.ctxcreator.ContextCreator
import compiler.desugarer.Desugarer
import compiler.io.SourceFile
import compiler.irs.Asts
import compiler.{AnalysisContext, CompilerStep, Mapper, MultiStep, TasksPipelines}
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import compiler.typechecker.TypeChecker

import java.nio.file.Path

object TestMain {

  def main(args: Array[String]): Unit = {

    val compilerTask1 = TasksPipelines.compiler(Path.of("testoutput"))
    compilerTask1.apply(List(SourceFile("examples/sorting.rsn")))

    val compilerTask2 = TasksPipelines.compiler(Path.of("testoutput"))
    compilerTask2.apply(List(SourceFile("examples/geometry.rsn")))

    val compilerTask3 = TasksPipelines.compiler(Path.of("testoutput"))
    compilerTask3.apply(List(SourceFile("examples/hello.rsn")))

  }

}
