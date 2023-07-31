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
  
  private def defaultExit(exitCode: ExitCode): Nothing = {
    System.exit(exitCode)
    throw new AssertionError("cannot happen")
  }

  /**
   * Pipeline for compilation (src file -> .class file)
   */
  def compiler(
                outputDirectoryPath: Path,
                javaVersionCode: Int,
                outputName: String,
                generateTests: Boolean,
                exit: ExitCode => Nothing = defaultExit
              ): CompilerStep[List[SourceCodeProvider], List[Path]] = {
    compilerImpl(outputDirectoryPath, Backend.BinaryMode, javaVersionCode, outputName, generateTests, exit)
  }

  /**
   * Pipeline for bytecode writing (src file -> asm text file)
   */
  def bytecodeWriter(
                      outputDirectoryPath: Path,
                      javaVersionCode: Int,
                      outputName: String,
                      exit: ExitCode => Nothing = defaultExit
                    ): CompilerStep[List[SourceCodeProvider], List[Path]] = {
    compilerImpl(outputDirectoryPath, Backend.AssemblyMode, javaVersionCode, outputName, true, exit)
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
                 exit: ExitCode => Nothing = defaultExit
               ): CompilerStep[SourceCodeProvider, Unit] = {
    val er = createErrorReporter(exit)
    frontend(er)
      .andThen(new PrettyPrinter(indentGranularity, displayAllParentheses))
      .andThen(new StringWriter(directoryPath, filename, er, overwriteFileCallback))
  }

  /**
   * Pipeline for typechecker (src file -> side effects of error reporting)
   */
  def typeChecker(exit: ExitCode => Nothing = defaultExit): CompilerStep[List[SourceCodeProvider], Unit] = {
    val er = createErrorReporter(exit)
    MultiStep(frontend(er))
      .andThen(new ContextCreator(er, FunctionsToInject.functionsToInject))
      .andThen(new TypeChecker(er))
      .andThen(Mapper(_ => println("no error found")))
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
               exit: ExitCode => Nothing = defaultExit
             ): CompilerStep[SourceCodeProvider, Unit] = {
    val er = createErrorReporter(exit)
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
                                              exit: ExitCode => Nothing) = {
    val er = createErrorReporter(exit)
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

  private def createErrorReporter(exit: ExitCode => Nothing): ErrorReporter =
    new ErrorReporter(errorsConsumer = System.err.println, exit = exit)

}
