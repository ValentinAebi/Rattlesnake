package compiler

import scala.runtime.Nothing$

object Errors {

  /**
   * Exit code used when aborting because of non fatal error(s)
   */
  val errorsExitCode = -20

  /**
   * Exit code used when aborting because of a fatal error
   */
  val fatalErrorExitCode = -21

  final case class CompilationError(compilationStep: CompilationStep, msg: String, posOpt: Option[Position]){
    override def toString: String =
      posOpt.map(pos => s"at $pos ").getOrElse("") ++ s"$msg [#$compilationStep]"
  }
  
  object CompilationError {
    def apply(compilationStep: CompilationStep, msg: String, pos: Position) =
      new CompilationError(compilationStep, msg, Some(pos))
  }

  /**
   * Container for errors found during compilation
   * @param errorConsumer to be called when errors are requested to be displayed
   */
  final class ErrorReporter(errorConsumer: (CompilationError | String) => Unit) {
    private var errors: List[CompilationError] = Nil

    /**
     * Add an error to the stack of non fatal errors
     */
    def push(error: CompilationError): Unit = {
      errors = error :: errors
    }
    
    def getErrors: List[CompilationError] = errors
    
    def displayErrors(): Unit = {
      for error <- errors.reverse do {
        errorConsumer(error)
      }
    }

    /**
     * Display errors and exit
     */
    def displayErrorsAndTerminate(): Nothing = {
      displayErrors()
      displayExitMessage()
      System.exit(if errors.isEmpty then 0 else errorsExitCode)

      // never executed
      throw new Error
    }

    /**
     * If there are errors, display them and terminate the program (o.w. do nothing)
     */
    def displayAndTerminateIfErrors(): Unit = {
      if (errors.nonEmpty){
        displayErrors()
        displayExitMessage()
        System.exit(errorsExitCode)
      }
    }

    /**
     * Display the given fatal error as well as all errors found until now and exit
     */
    def pushFatal(error: CompilationError): Nothing = {
      errorConsumer(s"FATAL: $error")
      if errors.nonEmpty then errorConsumer("Previously found errors:")
      displayErrors()
      displayExitMessage()
      System.exit(fatalErrorExitCode)

      // never executed
      throw new Error
    }
    
    private def displayExitMessage(): Unit = errorConsumer("Rattlesnake compiler exiting")

  }

}

