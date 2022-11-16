package compiler

import compiler.Errors.ErrorReporter
import compiler.backend.Backend
import compiler.ctxcreator.ContextCreator
import compiler.desugarer.Desugarer
import compiler.io.StringWriter
import compiler.irs.Asts
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import compiler.typechecker.TypeChecker
import org.objectweb.asm.ClassVisitor

import java.nio.file.Path

/**
 * Contains methods producing pipelines for different tasks, indicated by their name
 */
object TasksPipelines {

  /**
   * Pipeline for compilation (src file -> .class file)
   */
  def compiler(outputDirectoryPath: Path, javaVersionCode: Int, outputName: String): CompilerStep[List[SourceCodeProvider], List[Path]] = {
    compilerImpl(outputDirectoryPath, Backend.BinaryMode, javaVersionCode, outputName)
  }

  /**
   * Pipeline for bytecode writing (src file -> asm text file)
   */
  def bytecodeWriter(outputDirectoryPath: Path, javaVersionCode: Int, outputName: String): CompilerStep[List[SourceCodeProvider], List[Path]] = {
    compilerImpl(outputDirectoryPath, Backend.AssemblyMode, javaVersionCode, outputName)
  }

  /**
   * Pipeline for formatting (src file -> formatted text file)
   */
  def formatter(directoryPath: Path, filename: String,
                indentGranularity: Int, overwriteFileCallback: String => Boolean,
                displayAllParentheses: Boolean = false): CompilerStep[SourceCodeProvider, Unit] = {
    val er = createErrorReporter
    frontend(er)
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(directoryPath, filename, er, overwriteFileCallback))
  }

  /**
   * Pipeline for typechecker (src file -> side effects of error reporting)
   */
  val typeChecker: CompilerStep[List[SourceCodeProvider], Unit] = {
    val er = createErrorReporter
    MultiStep(frontend(er))
      .andThen(new ContextCreator(er))
      .andThen(new TypeChecker(er))
      .andThen(Mapper(_ => println("no error found")))
  }

  /**
   * Pipeline for desugaring (src file -> desugared src file)
   */
  def desugarer(outputDirectoryPath: Path, filename: String,
                indentGranularity: Int = 2, overwriteFileCallback: String => Boolean,
                displayAllParentheses: Boolean = false): CompilerStep[SourceCodeProvider, Unit] = {
    val er = createErrorReporter
    frontend(er)
      .andThen(Mapper(List(_)))
      .andThen(new ContextCreator(er))
      .andThen(new TypeChecker(er))
      .andThen(new Desugarer())
      .andThen(Mapper(_._1.head))
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(outputDirectoryPath, filename, er, overwriteFileCallback))
  }

  private def compilerImpl[V <: ClassVisitor](outputDirectoryPath: Path,
                                              backendMode: Backend.Mode[V],
                                              javaVersionCode: Int,
                                              outputName: String) = {
    val er = createErrorReporter
    MultiStep(frontend(er))
      .andThen(new ContextCreator(er))
      .andThen(new TypeChecker(er))
      .andThen(new Desugarer())
      .andThen(new Backend(backendMode, er, outputDirectoryPath, javaVersionCode, outputName))
  }

  private def frontend(er: ErrorReporter) = {
    new Lexer(er).andThen(new Parser(er))
  }

  private def createErrorReporter: ErrorReporter =
    new ErrorReporter(errorsConsumer = System.err.println)

}
