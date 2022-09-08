import compiler.Errors.ErrorReporter
import compiler.{CompilerStep, Mapper, MultiStep, SourceFile}
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.prettyprinter.PrettyPrinter
import compiler.typechecker.TypeChecker
import ctxcreator.ContextCreator

object TestMain {

  class Printer[T] extends CompilerStep[T, T]{
    override def apply(input: T): T = {
      println(input)
      input
    }
  }

  def main(args: Array[String]): Unit = {
    val er = new ErrorReporter(System.err.println)
    val frontend = MultiStep(new Lexer(er) andThen new Parser(er))
    val pipeline =
      frontend andThen new ContextCreator(er) andThen new TypeChecker(er) andThen Mapper(_._1) andThen MultiStep(new PrettyPrinter())
    val file = SourceFile("examples/sorting.rsn")
    val formatted = pipeline.apply(List(file))
    formatted.foreach(println)
  }

}
