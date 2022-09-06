package compiler.parser

import compiler.Errors.ErrorReporter
import compiler.SourceFile
import compiler.lexer.Lexer
import compiler.prettyprinter.PrettyPrinter

import org.junit.Test
import org.junit.Assert.assertEquals

class ParseAndReprintTests {

  @Test
  def parseAndReprintTest(): Unit = {
    val er = new ErrorReporter(System.err.println)
    val formatter = new Lexer(er) andThen new Parser(er) andThen new PrettyPrinter(indentGranularity = 4)
    val file = SourceFile("src/test/res/geometry.rsn")
    val formatted = formatter.apply(file)
    assertEquals(file.content.get, formatted)
  }

}
