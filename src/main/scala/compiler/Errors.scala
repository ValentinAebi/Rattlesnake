package compiler

import java.io.PrintStream
import scala.runtime.Nothing$

object Errors {

  /**
   * Exit code used when aborting because of non fatal error(s)
   */
  val errorsExitCode: Int = -20

  /**
   * Exit code used when aborting because of a fatal error
   */
  val fatalErrorExitCode: Int = -21

  /**
   * Compilation error or warning
   * 
   * Ordered according to their position in the program
   */
  sealed trait CompilationError extends Ordered[CompilationError] {
    val compilationStep: CompilationStep
    val msg: String
    val posOpt: Option[Position]

    val errorLevelDescr: String

    val isWarning: Boolean = isInstanceOf[Warning]
    val isFatal: Boolean = isInstanceOf[Fatal]

    override def compare(that: CompilationError): Int = {
      val stepComp = this.compilationStep.ordinal.compare(that.compilationStep.ordinal)
      if (stepComp != 0){
        stepComp
      } else {
        (this.posOpt, that.posOpt) match {
          case (None, None) => 0
          case (None, Some(_)) => 1
          case (Some(_), None) => -1
          case (Some(thisPos), Some(thatPos)) => thisPos.compare(thatPos)
        }
      }
    }

    override def toString: String = {
      val positionDescr = posOpt.map(pos => s"at $pos ").getOrElse("")
      s"[$errorLevelDescr] " ++ positionDescr ++ s"$msg #$compilationStep"
    }
  }

  /**
   * Non fatal error or warning
   */
  sealed trait NonFatal extends CompilationError

  /**
   * Fatal error, should terminate the compiler immediately
   */
  final case class Fatal(compilationStep: CompilationStep, msg: String, posOpt: Option[Position]) extends CompilationError {
    override val errorLevelDescr: String = "FATAL"
  }

  object Fatal {
    def apply(compilationStep: CompilationStep, msg: String, pos: Position) =
      new Fatal(compilationStep, msg, Some(pos))
  }

  /**
   * Non fatal error, should terminate the compiler at the end of the current compilation step
   */
  final case class Err(compilationStep: CompilationStep, msg: String, posOpt: Option[Position]) extends NonFatal {
    override val errorLevelDescr: String = "error"
  }

  object Err {
    def apply(compilationStep: CompilationStep, msg: String, pos: Position) =
      new Err(compilationStep, msg, Some(pos))
  }

  /**
   * Warning: should be reported to the user, but not terminate the compiler
   */
  final case class Warning(compilationStep: CompilationStep, msg: String, posOpt: Option[Position]) extends NonFatal {
    override val errorLevelDescr: String = "warning"
  }

  object Warning {
    def apply(compilationStep: CompilationStep, msg: String, pos: Position) =
      new Warning(compilationStep, msg, Some(pos))
  }

  type ErrorsConsumer = (CompilationError | String) => Unit

  /**
   * Container for errors found during compilation
   *
   * @param errorsConsumer to be called when errors need to be displayed
   */
  final class ErrorReporter(errorsConsumer: ErrorsConsumer) {
    private var errors: List[NonFatal] = Nil

    /**
     * Add an error to the stack of non fatal errors
     */
    def push(nonFatalError: NonFatal): Unit = {
      errors = nonFatalError :: errors
    }

    def getErrors: List[CompilationError] = errors

    def displayErrors(): Unit = {
      for error <- errors.sorted do {
        errorsConsumer(error)
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
      throw new AssertionError()
    }

    /**
     * If there are errors, display them and terminate the program (o.w. display the warnings and delete them)
     */
    def displayAndTerminateIfErrors(): Unit = {
      if (errors.exists(!_.isWarning)) {
        displayErrors()
        displayExitMessage()
        System.exit(errorsExitCode)
      }
      else {
        displayAndDeleteWarnings()
      }
    }

    /**
     * Display the given fatal error as well as all errors found until now and exit
     */
    def pushFatal(fatalError: Fatal): Nothing = {
      errorsConsumer(fatalError)
      if errors.nonEmpty then errorsConsumer("Previously found errors:")
      displayErrors()
      displayExitMessage()
      System.exit(fatalErrorExitCode)

      // never executed
      throw new AssertionError()
    }

    private def displayAndDeleteWarnings(): Unit = {
      for warning <- errors if warning.isWarning do {
        errorsConsumer(warning)
      }
      errors = errors.filterNot(_.isWarning)
    }

    private def displayExitMessage(): Unit = errorsConsumer("Rattlesnake compiler exiting")

  }

}

