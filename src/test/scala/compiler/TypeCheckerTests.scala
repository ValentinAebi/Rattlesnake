package compiler

import org.junit.Test
import Errors.*
import compiler.io.SourceFile
import compiler.typechecker.TypeChecker
import org.junit.Assert.{assertEquals, fail}

import scala.collection.mutable.ListBuffer


class TypeCheckerTests {

  private val srcDir = "src/test/res/typechecker-tests"

  @Test def modifiableArrayShouldNotBeCovariant(): Unit = {
    runAndExpectErrors("mut_covariance") {
      ErrorMatcher("modifiable array should not be covariant",
        line = 8, col = 9,
        msgMatcher = _.contains("expected 'mut arr arr Int', found 'mut arr mut arr Int'"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def shouldNotAssignNonModifToArrayOfModif(): Unit = {
    runAndExpectErrors("array_of_mut") {
      ErrorMatcher("array of modifiable array should not accept unmodifiable array",
        line = 6, col = 5,
        msgMatcher = _.contains("cannot assign a value of type 'arr Y' to an array of element type 'mut arr Y'"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def expectWarningForUnusedLocal(): Unit = {
    runAndExpectErrors("unused_local") {
      ErrorMatcher("expect warning for unused local",
        line = 2, col = 8,
        msgMatcher = _.contains("unused local: 'x' is never queried"),
        errorClass = classOf[Warning]
      )
    }
  }

  @Test def expectWarningForVarNotReassigned(): Unit = {
    runAndExpectErrors("var_never_reassigned") {
      ErrorMatcher("expect warning when variable is never reassigned",
        line = 4, col = 5,
        msgMatcher = _.contains("value declared as variable: 'k' could be a val"),
        errorClass = classOf[Warning]
      )
    }
  }

  @Test def expectWarningForUnnecessaryMut(): Unit = {
    runAndExpectErrors("unnecessary_mut") {
      ErrorMatcher("expect warning for unnecessary mut",
        line = 2, col = 17,
        msgMatcher = _.contains("unused modification privilege: 'a' could have type 'arr String'"),
        errorClass = classOf[Warning]
      )
    }
  }

  @Test def expectNoWarningWhenMutIsNecessaryBecauseOfSubcall(): Unit = {
    runAndExpectCorrect("seemingly_unnecessary_mut", failOnWarning = true)
  }
  
  @Test def expectErrorWhenMutNeededInFrontOfCall(): Unit = {
    runAndExpectErrors("mut_call_no"){
      ErrorMatcher("expect warning when call does not propagate mut",
        line = 8, col = 5,
        msgMatcher = _.contains("'y' should be of type 'mut arr Int', found 'arr Int'"),
        errorClass = classOf[Err]
      )
    }
  }
  
  @Test def expectNoErrorWhenMutInFrontOfCall(): Unit = {
    runAndExpectCorrect("mut_call_yes", failOnWarning = false)
  }

  private final case class ErrorMatcher(
                                         descr: String,
                                         private val line: Int = -1,
                                         private val col: Int = -1,
                                         private val msgMatcher: String => Boolean = null,
                                         compilationStep: CompilationStep = CompilationStep.TypeChecking,
                                         errorClass: Class[_ <: CompilationError]
                                       ) {

    private def _line: Option[Int] = Some(line).filter(_ >= 0)

    private def _col: Option[Int] = Some(col).filter(_ >= 0)

    private def _msgMatcher: Option[String => Boolean] = Some(msgMatcher).filter(_ != null)

    require(_line.isDefined || _col.isDefined || _msgMatcher.isDefined, "no criterion defined apart from compilation step")

    def doesMatch(err: CompilationError): Boolean = {
      _line.forall { l => err.posOpt.exists(_.line == l) }
        && _col.forall { c => err.posOpt.exists(_.col == c) }
        && _msgMatcher.forall { matcher => matcher(err.msg) }
        && err.compilationStep == compilationStep
        && err.getClass == errorClass
    }
  }

  private def runAndExpectErrors(srcFileName: String)(errorsMatchers: ErrorMatcher*): Unit = {

    val allEncounteredErrors = ListBuffer.empty[CompilationError]
    val remainingErrorMatchers = ListBuffer.from(errorsMatchers)

    val errorsConsumer: ErrorsConsumer = {
      case _: String => ()
      case err: CompilationError =>
        allEncounteredErrors.addOne(err)
        remainingErrorMatchers.zipWithIndex
          .find { (matcher, _) => matcher.doesMatch(err) }
          .foreach { (_, idx) =>
            remainingErrorMatchers.remove(idx)
          }
    }

    final case class ExitException(exitCode: ExitCode) extends Exception

    val er = new ErrorReporter(errorsConsumer, exit = code => throw ExitException(code))
    val tc = TasksPipelines.typeChecker(er, okReporter = _ => ())
    val testFile = SourceFile(s"$srcDir/$srcFileName.${FileExtensions.rattlesnake}")
    try {
      tc.apply(List(testFile))
    } catch case ExitException(code) => ()
    if (remainingErrorMatchers.nonEmpty) {
      val msg =
        remainingErrorMatchers.map(_.descr).mkString("no error found that matches the following matchers: \n", "\n", "")
          ++ "\n\nThe following errors were reported by typechecking:\n"
          ++ allEncounteredErrors.mkString("\n") ++ "\n"
      fail(msg)
    }
  }

  private def runAndExpectCorrect(srcFileName: String, failOnWarning: Boolean): Unit = {
    val warnings = ListBuffer.empty[Warning]
    val errorsConsumer: ErrorsConsumer = {
      case w: Warning => warnings.addOne(w)
      case _ => ()
    }
    val er = new ErrorReporter(errorsConsumer, exit = failExit)
    val tc = TasksPipelines.typeChecker(er, okReporter = _ => ())
    val testFile = SourceFile(s"$srcDir/$srcFileName.${FileExtensions.rattlesnake}")
    tc.apply(List(testFile))
    if (warnings.nonEmpty && failOnWarning){
      fail("Warnings found:\n" ++ warnings.mkString("\n"))
    }
  }

  private def failExit(exitCode: ExitCode): Nothing = {
    fail(s"exit called, exit code: $exitCode")
    throw new AssertionError("cannot happen")
  }

}
