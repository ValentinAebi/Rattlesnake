package compiler

import compiler.Errors.{CompilationError, ErrorReporter}
import compiler.ctxcreator.ContextCreator
import compiler.desugarer.Desugarer
import compiler.io.SourceFile
import compiler.irs.Asts.{FunDef, Source}
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.typechecker.TypeChecker
import identifiers.StringEqualityFunId

import scala.util.{Success, Try}

/**
 * Handles the functions that should be added to any program
 *
 * Currently only `$stringEq`, which is called when comparing strings using `==`
 */
object FunctionsToInject {

  private val stringEqualityCodeProvider = new SourceCodeProvider {
    override def lines: Try[Seq[String]] = Success(
      """fn stringEq(s1: String, s2: String) -> Bool {
        |   if #s1 != #s2 {
        |      return false
        |   };
        |   val arr1 = toCharArray(s1);
        |   val arr2 = toCharArray(s2);
        |   for var i = 0; i < #s1; i += 1 {
        |      if arr1[i] != arr2[i] {
        |         return false
        |      }
        |   };
        |   return true
        |}""".stripMargin.lines().toArray().toList.map(_.asInstanceOf[String])
    )

    override def name: String = StringEqualityFunId.stringId
  }

  private val stringEqualityFunction: FunDef = {

    val errorsConsumer: (CompilationError | String) => Nothing = _ => assert(false)
    val er = new ErrorReporter(errorsConsumer)

    val pipeline =
      new Lexer(er)
        .andThen(new Parser(er))
        .andThen(Mapper(List(_)))
        .andThen(new ContextCreator(er, functionsToInject = Nil))
        .andThen(new TypeChecker(er))
        .andThen(new Desugarer())

    // load code of the function, desugar it and replace its name
    val resFile = stringEqualityCodeProvider
    val result = pipeline.apply(resFile)
    val (List(Source(List(funDef: FunDef @unchecked))), _) = result
    assert(funDef.funName.stringId == StringEqualityFunId.rawName)
    funDef.copy(funName = StringEqualityFunId)
  }

  val functionsToInject: List[FunDef] = List(stringEqualityFunction)

}
