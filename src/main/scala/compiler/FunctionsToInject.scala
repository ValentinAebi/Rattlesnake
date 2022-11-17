package compiler

import compiler.Errors.{CompilationError, ErrorReporter}
import compiler.ctxcreator.ContextCreator
import compiler.desugarer.Desugarer
import compiler.io.SourceFile
import compiler.irs.Asts.{FunDef, Source}
import compiler.lexer.Lexer
import compiler.parser.Parser
import compiler.typechecker.TypeChecker

/**
 * Handles the functions that should be added to any program
 *
 * Currently only `$stringEq`, which is called when comparing strings using `==`
 */
object FunctionsToInject {

  val stringEqualityMethodName = "$stringEq"

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
    val resFile = SourceFile("res/streq.rsn")
    val result = pipeline.apply(resFile)
    val (List(Source(List(funDef: FunDef@unchecked))), _) = result
    assert(funDef.funName == stringEqualityMethodName.tail)
    funDef.copy(funName = stringEqualityMethodName)
  }

  val functionsToInject: List[FunDef] = List(stringEqualityFunction)

}
