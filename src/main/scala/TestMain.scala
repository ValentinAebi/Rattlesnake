import compiler.Errors.ErrorReporter
import compiler.{CompilerStep, SourceFile}
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter

object TestMain {

  class Printer[T] extends CompilerStep[T, T]{
    override def apply(input: T): T = {
      println(input)
      input
    }
  }

  def main(args: Array[String]): Unit = {
    val er = new ErrorReporter(System.err.println)
    val formatter = new Lexer(er) andThen new Parser(er) andThen new PrettyPrinter()
    val file = SourceFile("examples/geometry.rsn")
    val formatted = formatter.apply(file)
    println(formatted)
  }

}
