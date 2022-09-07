import compiler.Errors.ErrorReporter
import compiler.{CompilerStep, MultiStep, SourceFile}
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import compiler.typechecker.TypeChecker

object TestMain {

  class Printer[T] extends CompilerStep[T, T]{
    override def apply(input: T): T = {
      println(input)
      input
    }
  }

  def main(args: Array[String]): Unit = {
    val er = new ErrorReporter(System.err.println)
    val pipeline = MultiStep(new Lexer(er) andThen new Parser(er)) andThen new TypeChecker(er) andThen MultiStep(new PrettyPrinter())
    val file = SourceFile("examples/geometry.rsn")
    val formatted = pipeline.apply(List(file))
    formatted.foreach(println)
  }

}
