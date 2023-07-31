package compiler

import compiler.Errors.{ErrorReporter, ExitCode}
import compiler.io.SourceFile
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import org.junit.Assert.{assertEquals, fail}
import org.junit.Test

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
    val file = SourceFile("src/test/res/geometry.rsn")
    val actualRes = formatter.apply(file)
    val expectedRes = filterOutCommentLines(file.content.get)
    assertEquals(expectedRes, actualRes)
  }

  private def filterOutCommentLines(str: String): String = {
    str.lines().filter { line =>
      !line.trim.startsWith("//")
    }.toArray.mkString("\n")
  }

}
