package compiler

import compiler.Errors.ErrorReporter
import compiler.ctxcreator.ContextCreator
import compiler.desugarer.Desugarer
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.typechecker.TypeChecker
import compiler.backend.Backend
import compiler.io.StringWriter
import compiler.irs.Asts
import compiler.prettyprinter.PrettyPrinter
import org.objectweb.asm.ClassVisitor

import java.nio.file.Path

object TasksPipelines {

  def compiler(outputDirectoryPath: Path, javaVersionCode: Int, optName: Option[String] = None): CompilerStep[List[SourceCodeProvider], List[Path]] = {
    compilerImpl(outputDirectoryPath, Backend.BinaryMode, javaVersionCode, optName)
  }

  def bytecodeWriter(outputDirectoryPath: Path, javaVersionCode: Int, optName: Option[String] = None): CompilerStep[List[SourceCodeProvider], List[Path]] = {
    compilerImpl(outputDirectoryPath, Backend.AssemblyMode, javaVersionCode, optName)
  }

  def formatter(directoryPath: Path, filename: String,
                indentGranularity: Int = 2, displayAllParentheses: Boolean = false): CompilerStep[SourceCodeProvider, Unit] = {
    val er = new ErrorReporter(System.err.println)
    frontend(er)
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(directoryPath, filename, er))
  }

  val typeChecker: CompilerStep[List[SourceCodeProvider], Unit] = {
    val er = new ErrorReporter(System.err.println)
    MultiStep(frontend(er))
      .andThen(new ContextCreator(er))
      .andThen(new TypeChecker(er))
      .andThen(Mapper(_ => println("no error found")))
  }

  def desugarer(outputDirectoryPath: Path, filename: String,
                indentGranularity: Int = 2, displayAllParentheses: Boolean = false): CompilerStep[SourceCodeProvider, Unit] = {
    val er = new ErrorReporter(System.err.println)
    frontend(er)
      .andThen(Mapper(src => (List(src), null)))
      .andThen(new Desugarer())
      .andThen(Mapper(_._1.head))
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(outputDirectoryPath, filename, er))
  }

  private def compilerImpl[V <: ClassVisitor](outputDirectoryPath: Path,
                                              backendMode: Backend.Mode[V],
                                              javaVersionCode: Int,
                                              optName: Option[String]) = {
    val er = new ErrorReporter(System.err.println)
    MultiStep(frontend(er))
      .andThen(new ContextCreator(er))
      .andThen(new TypeChecker(er))
      .andThen(new Desugarer())
      .andThen(new Backend(backendMode, er, outputDirectoryPath, javaVersionCode, optName))
  }

  private def frontend(er: ErrorReporter) = {
    new Lexer(er).andThen(new Parser(er))
  }

}
