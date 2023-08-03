package compiler

import org.junit.Test
import Errors.*
import compiler.io.SourceFile
import compiler.typechecker.TypeChecker
import org.junit.Assert.fail

import scala.collection.mutable.ListBuffer


class TypeCheckerTests {

  private val srcDir = "src/test/res/should-fail"


  @Test def modifiableArrayShouldNotBeCovariant(): Unit = {
    performTest("mut_covariance") {
      ErrorMatcher("modifiable array should not be covariant",
        line = 8, col = 9,
        msgMatcher = _.contains("expected 'mut arr arr Int', found 'mut arr mut arr Int'")
      )
    }
  }

  @Test def shouldNotAssignNonModifToArrayOfModif(): Unit = {
    performTest("array_of_mut"){
      ErrorMatcher("array of modifiable array should not accept unmodifiable array",
        line = 6, col = 5,
        msgMatcher = _.contains("cannot assign a value of type 'arr Y' to an array of element type 'mut arr Y'")
      )
    }
  }

  private final case class ErrorMatcher(
                                         descr: String,
                                         private val line: Int = -1,
                                         private val col: Int = -1,
                                         private val msgMatcher: String => Boolean = null,
                                         compilationStep: CompilationStep = CompilationStep.TypeChecking,
                                         errorClass: Class[_ <: CompilationError] = classOf[Err]
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

  private def performTest(srcFileName: String)(errorsMatchers: ErrorMatcher*): Unit = {

    val remainingErrorMatchers = ListBuffer.from(errorsMatchers)

    val errorsConsumer: ErrorsConsumer = {
      case _: String => ()
      case err: CompilationError =>
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
          ++ "\n\nThe following errors were reported by typechecking:\n" ++ er.getErrors.mkString("\n") ++ "\n"
      fail(msg)
    }
  }

}
