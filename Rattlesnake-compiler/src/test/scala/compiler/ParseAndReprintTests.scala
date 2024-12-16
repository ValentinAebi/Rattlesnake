package compiler

import compiler.reporting.Errors.{ErrorReporter, ExitCode}
import compiler.io.SourceFile
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import org.junit.Assert.{assertEquals, fail}
import org.junit.{ComparisonFailure, Test}

class ParseAndReprintTests {

  private def failExit(exitCode: ExitCode): Nothing = {
    fail(s"exit called, exit code: $exitCode")
    throw new AssertionError("cannot happen")
  }

  @Test
  def parseAndReprintTest(): Unit = {

    def failTestOnAttemptToPrintSomething(any: Any): Unit = {
      fail(s"Unexpected text:\n$any")
    }

    val er = new ErrorReporter(failTestOnAttemptToPrintSomething, failExit)
    val formatter = new Lexer(er) andThen new Parser(er) andThen new PrettyPrinter(indentGranularity = 4)
    val file = SourceFile("src/test/res/should-pass/geometry.rsn")
    val actualRes = formatter.apply(file)
    val expectedRes = filterOutCommentLines(file.content.get)
    assertEqualsExceptEmptyLines(expectedRes, actualRes)
  }

  private def filterOutCommentLines(str: String): String = {
    str.lines().filter { line =>
      !line.trim.startsWith("//")
    }.toArray.mkString("\n")
  }

  private def assertEqualsExceptEmptyLines(exp: String, act: String): Unit = {
    val expLines = exp.trim.lines().toArray(new Array[String](_))
    val actLines = act.trim.lines().toArray(new Array[String](_))
    if (expLines.length != actLines.length){
      throw new ComparisonFailure("", exp, act)
    } else {
      for ((e, a) <- expLines.zip(actLines)) {
        if (e.trim.nonEmpty && a.trim.nonEmpty && e != a){
          throw new ComparisonFailure("", exp, act)
        }
      }
    }
  }

}
