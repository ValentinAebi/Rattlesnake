package compiler

import compiler.CompilationStep.ContextCreation
import compiler.Errors.*
import compiler.TasksPipelines.frontend
import compiler.ctxcreator.ContextCreator
import compiler.io.SourceFile
import compiler.lowerer.Lowerer
import compiler.typechecker.TypeChecker
import org.junit.Assert.fail
import org.junit.Test

import scala.collection.mutable.ListBuffer


class AnalyzerTests {

  private val srcDir = "src/test/res/analyzer-tests"

  @Test def expectWarningForUnusedLocal(): Unit = {
    runAndExpectErrors("unused_local") {
      ErrorMatcher("expect warning for unused local",
        line = 4, col = 12,
        msgMatcher = _.contains("unused local: 'x' is never queried"),
        errorClass = classOf[Warning]
      )
    }
  }

  @Test def expectWarningForVarNotReassigned(): Unit = {
    runAndExpectErrors("var_never_reassigned") {
      ErrorMatcher("expect warning when variable is never reassigned",
        line = 6, col = 9,
        msgMatcher = _.contains("value declared as variable: 'k' could be a val"),
        errorClass = classOf[Warning]
      )
    }
  }

  @Test def typingErrorsInSortingProgram(): Unit = {
    runAndExpectErrors("sorting_bad_types", matchersListShouldBeComplete = false)(
      ErrorMatcher("i2 is String",
        line = 6, col = 21,
        msgMatcher = _.contains("expected 'Int', found 'String'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("swap expects an array of Double as its first argument",
        line = 13, col = 21,
        msgMatcher = _.contains("expected 'mut arr Double', found 'mut arr Int'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("type Index is unknown",
        line = 18, col = 31,
        msgMatcher = _.contains("unknown type: 'Index'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("type Baz is unknown",
        line = 19, col = 17,
        msgMatcher = _.contains("not found: structure or module 'Baz'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("panic expects String, not Int",
        line = 21, col = 13,
        msgMatcher = _.contains("expected 'String', found 'Int'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("#array is Int",
        line = 24, col = 17,
        msgMatcher = _.contains("expected 'Char', found 'Int'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("expect Int, not String",
        line = 26, col = 21,
        msgMatcher = _.contains("expected 'Int', found 'String'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("Fake is unknown",
        line = 34, col = 19,
        msgMatcher = _.contains("unknown type: 'Fake'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("no operator Int + String -> Int",
        line = 35, col = 32,
        msgMatcher = _.contains("no definition of operator '+' found for operands 'Int' and 'String'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("expect array of Int, not array of String",
        line = 39, col = 26,
        msgMatcher = _.contains("expected 'mut arr Int', found 'mut arr String'"),
        errorClass = classOf[Err]
      )
    )
  }

  @Test def shouldRejectModifOfConstField(): Unit = {
    runAndExpectErrors("modif_const_field"){
      ErrorMatcher("should reject modification of constant field",
        line = 10, col = 9,
        msgMatcher = _.contains("cannot update immutable field"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def shouldRejectCyclicSubtyping(): Unit = {
    runAndExpectErrors("cyclic_subtyping"){
      ErrorMatcher("should reject cycle in subtyping relation",
        msgMatcher = _.contains("cyclic subtyping"),
        errorClass = classOf[Err],
        compilationStep = ContextCreation
      )
    }
  }

  @Test def shouldRejectUnrelatedEqualities(): Unit = {
    runAndExpectErrors("subtype_equality_check")(
      ErrorMatcher("should reject second argument in test1",
        line = 5, col = 60,
        msgMatcher = _.contains("expected 'S', found 'I'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("should reject first argument in test2",
        line = 9, col = 41,
        msgMatcher = _.contains("expected 'S', found 'I'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("should reject first argument in test3 (second argument is thus ignored)",
        line = 13, col = 41,
        msgMatcher = _.contains("expected 'S', found 'I'"),
        errorClass = classOf[Err]
      )
    )
  }

  @Test def explicitCastTest(): Unit = {
    runAndExpectErrors("explicit_cast")(
      ErrorMatcher("should reject unrelated cast to String",
        line = 26, col = 17,
        msgMatcher = _.contains("cannot cast 'Sub' to 'String'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("should warn on cast to supertype",
        line = 28, col = 17,
        msgMatcher = _.contains("useless conversion: 'Sub' --> 'Super'"),
        errorClass = classOf[Warning]
      ),
      ErrorMatcher("should reject unrelated cast of struct to struct",
        line = 32, col = 17,
        msgMatcher = _.contains("cannot cast 'Sub' to 'Indep'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("should reject unrelated cast of interface to struct",
        line = 33, col = 17,
        msgMatcher = _.contains("cannot cast 'Super' to 'Indep'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("should reject unrelated cast of struct to interface",
        line = 35, col = 17,
        msgMatcher = _.contains("cannot cast 'Sub' to 'I'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("should reject unrelated cast of interface to interface",
        line = 36, col = 17,
        msgMatcher = _.contains("cannot cast 'Super' to 'I'"),
        errorClass = classOf[Err]
      )
    )
  }

  @Test def complexHierarchyTest(): Unit = {
    runAndExpectErrors("complex_hierarchy")(
      ErrorMatcher("should reject non-var field overriding var field (1)",
        line = 6, col = 1,
        msgMatcher = _.contains("subtyping error: value needs to be reassignable in subtypes of Tree"),
        errorClass = classOf[Err],
        compilationStep = ContextCreation
      ),
      ErrorMatcher("should reject field override with unrelated types",
        line = 55, col = 1,
        msgMatcher = _.contains("subtyping error: type of msg in MessageableImplWrong should be a subtype of its type in Messageable2"),
        errorClass = classOf[Err],
        compilationStep = ContextCreation
      ),
      ErrorMatcher("should reject non-var field overriding var field (2)",
        line = 67, col = 1,
        msgMatcher = _.contains("subtyping error: value needs to be reassignable in subtypes of CellWithMemoLastAndInit"),
        errorClass = classOf[Err],
        compilationStep = ContextCreation
      ),
      ErrorMatcher("should reject non-mutable field overriding mutable field",
        line = 67, col = 1,
        msgMatcher = _.contains("subtyping error: type of msg in CellWithMemoLastInitAndMsg should be a subtype of its type in Messageable1"),
        errorClass = classOf[Err],
        compilationStep = ContextCreation
      ),
      ErrorMatcher("should reject covariant var field with covariant types",
        line = 84, col = 1,
        msgMatcher = _.contains("subtyping error: type of tree is not the same in TreeContainerImpl and TreeContainer"),
        errorClass = classOf[Err],
        compilationStep = ContextCreation
      ),
      ErrorMatcher("should reject subtyping with missing field",
        line = 84, col = 1,
        msgMatcher = _.contains("missing field versionId"),
        errorClass = classOf[Err],
        compilationStep = ContextCreation
      )
    )
  }
  
  @Test def smartcastAnd(): Unit = {
    runAndExpectErrors("smartcast_and")(
      ErrorMatcher("access to possibly missing field should be rejected",
        line = 7, col = 13,
        msgMatcher = _.contains("struct 'Option' has no field named 'value'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("access to value on value smartcasted to None should be rejected",
        line = 8, col = 28,
        msgMatcher = _.contains("struct 'None' has no field named 'value'"),
        errorClass = classOf[Err]
      )
    )
  }

  private final case class ErrorMatcher(
                                         descr: String,
                                         private val line: Int = -1,
                                         private val col: Int = -1,
                                         private val msgMatcher: String => Boolean = null,
                                         compilationStep: CompilationStep = CompilationStep.TypeChecking,
                                         errorClass: Class[? <: CompilationError]
                                       ) {

    private def _line: Option[Int] = Some(line).filter(_ >= 0)

    private def _col: Option[Int] = Some(col).filter(_ >= 0)

    private def _msgMatcher: Option[String => Boolean] = Some(msgMatcher).filter(_ != null)

    private def _compilationStep: Option[CompilationStep] = Some(compilationStep).filter(_ != null)

    require(_line.isDefined || _col.isDefined || _msgMatcher.isDefined, "no criterion defined apart from compilation step")

    def doesMatch(err: CompilationError): Boolean = {
      _line.forall { l => err.posOpt.exists(_.line == l) }
        && _col.forall { c => err.posOpt.exists(_.col == c) }
        && _msgMatcher.forall { matcher => matcher(err.msg) }
        && _compilationStep.forall(_ == err.compilationStep)
        && err.getClass == errorClass
    }

    def details: String = s"$descr ($line:$col)"

  }

  private def warningMatcher(line: Int, keywords: String*): ErrorMatcher = ErrorMatcher(
    "warning matcher with keyword(s) " ++ keywords.map("'" + _ + "'").mkString(", "),
    line = line, errorClass = classOf[Warning], msgMatcher = msg => keywords.forall(msg.contains(_)),
    compilationStep = null
  )

  private def runAndExpectErrors(srcFileName: String, matchersListShouldBeComplete: Boolean = true)(errorsMatchers: ErrorMatcher*): Unit = {

    val allEncounteredErrors = ListBuffer.empty[CompilationError]
    val unmatchedErrors = ListBuffer.empty[CompilationError]
    val remainingErrorMatchers = ListBuffer.from(errorsMatchers)

    val errorsConsumer: ErrorsConsumer = {
      case _: String => ()
      case err: CompilationError =>
        allEncounteredErrors.addOne(err)
        remainingErrorMatchers.zipWithIndex
          .find { (matcher, _) => matcher.doesMatch(err) }
          .orElse {
            unmatchedErrors.addOne(err)
            None
          }.foreach { (_, idx) =>
            remainingErrorMatchers.remove(idx)
          }
    }

    final case class ExitException(exitCode: ExitCode) extends Exception

    val er = new ErrorReporter(errorsConsumer, exit = code => throw ExitException(code))
    val pipeline =
      MultiStep(frontend(er))
        .andThen(new ContextCreator(er))
        .andThen(new TypeChecker(er))
        .andThen(new Lowerer())
    val testFile = SourceFile(s"$srcDir/$srcFileName.${FileExtensions.rattlesnake}")
    try {
      pipeline.apply(List(testFile))
    } catch case ExitException(code) => ()

    if (remainingErrorMatchers.nonEmpty) {
      val msg =
        "No error found that matches the following matchers:\n"
          ++ remainingErrorMatchers.map(_.details).mkString("\n")
          ++ "\n\nThe following errors were reported by typechecking:\n"
          ++ allEncounteredErrors.mkString("\n") ++ "\n"
      fail(msg)
    } else if (matchersListShouldBeComplete && unmatchedErrors.nonEmpty){
      val msg =
        "Errors have been found for all matchers, but there were additional one(s):\n"
          ++ unmatchedErrors.mkString("\n")
      fail(msg)
    }
  }

  private def runAndExpectCorrect(srcFileName: String, failOnWarning: Boolean): Unit = {
    val warnings = ListBuffer.empty[Warning]
    val errorsConsumer: ErrorsConsumer = {
      case w: Warning => warnings.addOne(w)
      case _ => ()
    }
    var er: ErrorReporter = null
    er = new ErrorReporter(errorsConsumer, exit = failExit(er))
    val tc = TasksPipelines.typeChecker(er, okReporter = _ => ())
    val testFile = SourceFile(s"$srcDir/$srcFileName.${FileExtensions.rattlesnake}")
    tc.apply(List(testFile))
    if (warnings.nonEmpty && failOnWarning){
      fail("Warnings found:\n" ++ warnings.mkString("\n"))
    }
  }

  private def failExit(errorReporter: ErrorReporter)(exitCode: ExitCode): Nothing = {
    fail(s"exit called, exit code: $exitCode\nErrors found:\n" ++ errorReporter.getErrors.mkString("\n"))
    throw new AssertionError("cannot happen")
  }

}
