package compiler

import compiler.Errors.{ErrorReporter, ExitCode, errorsExitCode}
import compiler.backend.Backend
import compiler.ctxcreator.ContextCreator
import compiler.lowerer.Lowerer
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
  def compiler(
                outputDirectoryPath: Path,
                javaVersionCode: Int,
                outputName: String,
                generateTests: Boolean,
                er: ErrorReporter = defaultErrorReporter
              ): CompilerStep[List[SourceCodeProvider], List[Path]] = {
    compilerImpl(outputDirectoryPath, Backend.BinaryMode, javaVersionCode, outputName, generateTests, er)
  }

  /**
   * Pipeline for bytecode writing (src file -> asm text file)
   */
  def bytecodeWriter(
                      outputDirectoryPath: Path,
                      javaVersionCode: Int,
                      outputName: String,
                      er: ErrorReporter = defaultErrorReporter
                    ): CompilerStep[List[SourceCodeProvider], List[Path]] = {
    compilerImpl(outputDirectoryPath, Backend.AssemblyMode, javaVersionCode, outputName, true, er)
  }

  /**
   * Pipeline for formatting (src file -> formatted text file)
   */
  def formatter(
                 directoryPath: Path, 
                 filename: String, 
                 indentGranularity: Int,
                 overwriteFileCallback: String => Boolean, 
                 displayAllParentheses: Boolean = false,
                 er: ErrorReporter = defaultErrorReporter
               ): CompilerStep[SourceCodeProvider, Unit] = {
    frontend(er)
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(directoryPath, filename, er, overwriteFileCallback))
  }

  /**
   * Pipeline for typechecker (src file -> side effects of error reporting)
   */
  def typeChecker(er: ErrorReporter = defaultErrorReporter, okReporter: String => Unit = println): CompilerStep[List[SourceCodeProvider], Unit] = {
    MultiStep(frontend(er))
      .andThen(new ContextCreator(er, FunctionsToInject.functionsToInject))
      .andThen(new TypeChecker(er))
      .andThen(Mapper(_ => okReporter("no error found")))
  }

  /**
   * Pipeline for lowering (src file -> lowered src file)
   */
  def lowerer(
               outputDirectoryPath: Path,
               filename: String, 
               indentGranularity: Int = 2,
               overwriteFileCallback: String => Boolean, 
               displayAllParentheses: Boolean = false,
               er: ErrorReporter = defaultErrorReporter
             ): CompilerStep[SourceCodeProvider, Unit] = {
    frontend(er)
      .andThen(Mapper(List(_)))
      .andThen(new ContextCreator(er, FunctionsToInject.functionsToInject))
      .andThen(new TypeChecker(er))
      .andThen(new Lowerer())
      .andThen(Mapper(_._1.head))
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(outputDirectoryPath, filename, er, overwriteFileCallback))
  }

  private def compilerImpl[V <: ClassVisitor](outputDirectoryPath: Path,
                                              backendMode: Backend.Mode[V],
                                              javaVersionCode: Int,
                                              outputName: String,
                                              generateTests: Boolean,
                                              er: ErrorReporter) = {
    MultiStep(frontend(er))
      .andThen(new ContextCreator(er, FunctionsToInject.functionsToInject))
      .andThen(new TypeChecker(er))
      .andThen(new Lowerer())
      .andThen(new Backend(
          backendMode, er, outputDirectoryPath, javaVersionCode, outputName,
          FunctionsToInject.functionsToInject, generateTests
      ))
  }

  private def frontend(er: ErrorReporter) = {
    new Lexer(er).andThen(new Parser(er))
  }

  private def defaultErrorReporter: ErrorReporter =
    new ErrorReporter(errorsConsumer = System.err.print, exit = defaultExit)

  private def defaultExit(exitCode: ExitCode): Nothing = {
    System.exit(exitCode)
    throw new AssertionError("cannot happen")
  }

}
