package compiler

import org.junit.Test
import Errors.*
import compiler.CompilationStep.ContextCreation
import compiler.io.SourceFile
import compiler.typechecker.TypeChecker
import org.junit.Assert.{assertEquals, fail}

import scala.collection.mutable.ListBuffer


class TypeCheckerTests {

  private val srcDir = "src/test/res/typechecker-tests"

  @Test def modifiableArrayShouldNotBeCovariant(): Unit = {
    runAndExpectErrors("mut_covariance") {
      ErrorMatcher("modifiable array should not be covariant",
        line = 8, col = 9,
        msgMatcher = _.contains("expected 'mut arr arr Int', found 'mut arr mut arr Int'"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def shouldNotAssignNonModifToArrayOfModif(): Unit = {
    runAndExpectErrors("array_of_mut") {
      ErrorMatcher("array of modifiable array should not accept unmodifiable array",
        line = 6, col = 5,
        msgMatcher = _.contains("expected 'mut arr Y', found 'arr Y'"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def expectWarningForUnusedLocal(): Unit = {
    runAndExpectErrors("unused_local") {
      ErrorMatcher("expect warning for unused local",
        line = 2, col = 8,
        msgMatcher = _.contains("unused local: 'x' is never queried"),
        errorClass = classOf[Warning]
      )
    }
  }

  @Test def expectWarningForVarNotReassigned(): Unit = {
    runAndExpectErrors("var_never_reassigned") {
      ErrorMatcher("expect warning when variable is never reassigned",
        line = 4, col = 5,
        msgMatcher = _.contains("value declared as variable: 'k' could be a val"),
        errorClass = classOf[Warning]
      )
    }
  }

  @Test def expectWarningForUnnecessaryMut(): Unit = {
    runAndExpectErrors("unnecessary_mut") {
      ErrorMatcher("expect warning for unnecessary mut",
        line = 2, col = 17,
        msgMatcher = _.contains("unused modification privilege: 'a' could have type 'arr String'"),
        errorClass = classOf[Warning]
      )
    }
  }

  @Test def expectNoWarningWhenMutIsNecessaryBecauseOfSubcall(): Unit = {
    runAndExpectCorrect("seemingly_unnecessary_mut", failOnWarning = true)
  }
  
  @Test def expectNoErrorWhenMutInFrontOfCall(): Unit = {
    runAndExpectCorrect("mut_call", failOnWarning = false)
  }

  @Test def expectErrorWhenMutLeakOnLocal(): Unit = {
    runAndExpectErrors("mut_leak_local"){
      ErrorMatcher("local with mut type should be rejected when no mut permission",
        line = 3, col = 5,
        msgMatcher = _.contains("expected 'mut arr Int', found 'arr Int'"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def expectErrorWhenMutLeakOnParam(): Unit = {
    runAndExpectErrors("mut_leak_param") {
      ErrorMatcher("param with mut type should be rejected when no mut permission",
        line = 8, col = 9,
        msgMatcher = _.contains("expected 'mut arr String', found 'arr String'"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def expectErrorWhenMutLeakOnRet(): Unit = {
    runAndExpectErrors("mut_leak_ret"){
      ErrorMatcher("mutation on returned unmodifiable array should be rejected",
        line = 8, col = 5,
        msgMatcher = _.contains("update impossible: missing modification privileges on array"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def expectErrorWhenMutLeakOnFieldGet(): Unit = {
    runAndExpectErrors("mut_leak_fld_get"){
      ErrorMatcher("modifying an unmodifiable struct gotten as a field should be rejected",
        line = 12, col = 5,
        msgMatcher = _.contains("cannot update field 'x': missing modification privileges on owner struct"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def expectErrorWhenMutLeakOnFieldInit(): Unit = {
    runAndExpectErrors("mut_leak_fld_init"){
      ErrorMatcher("passing an unmodifiable struct as a mut field of another struct should result in an error",
        line = 8, col = 23,
        msgMatcher = _.contains("expected 'mut arr Int', found 'arr Int'"),
        errorClass = classOf[Err]
      )
    }
  }

  @Test def typingErrorsInSortingProgram(): Unit = {
    runAndExpectErrors("sorting_bad_types")(
      ErrorMatcher("i2 is String",
        line = 4, col = 17,
        msgMatcher = _.contains("expected 'Int', found 'String'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("swap expects an array of Double as its first argument",
        line = 11, col = 14,
        msgMatcher = _.contains("expected 'mut arr Double', found 'mut arr Int'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("type Index is unknown",
        line = 16, col = 27,
        msgMatcher = _.contains("unknown type: 'Index'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("no struct Baz known",
        line = 17, col = 13,
        msgMatcher = _.contains("not found: struct 'Baz'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("panic expects String, not Int",
        line = 19, col = 9,
        msgMatcher = _.contains("expected 'String', found 'Int'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("#array is Int",
        line = 22, col = 13,
        msgMatcher = _.contains("expected 'Char', found 'Int'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("expect Int, not String",
        line = 24, col = 17,
        msgMatcher = _.contains("expected 'Int', found 'String'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("Fake is unknown",
        line = 32, col = 15,
        msgMatcher = _.contains("unknown type: 'Fake'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("no operator Int + String -> Int",
        line = 33, col = 28,
        msgMatcher = _.contains("no definition of operator '+' found for operands 'Int' and 'String'"),
        errorClass = classOf[Err]
      ),
      ErrorMatcher("expect array of Int, not array of String",
        line = 37, col = 19,
        msgMatcher = _.contains("expected 'mut arr Int', found 'mut arr String'"),
        errorClass = classOf[Err]
      )
    )
  }

  @Test def shouldRejectModifOfConstField(): Unit = {
    runAndExpectErrors("modif_const_field"){
      ErrorMatcher("should reject modification of constant field",
        line = 8, col = 5,
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

  private final case class ErrorMatcher(
                                         descr: String,
                                         private val line: Int = -1,
                                         private val col: Int = -1,
                                         private val msgMatcher: String => Boolean = null,
                                         compilationStep: CompilationStep = CompilationStep.TypeChecking,
                                         errorClass: Class[_ <: CompilationError]
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

    def details: String = s"$descr ($line:$col)"

  }

  private def runAndExpectErrors(srcFileName: String)(errorsMatchers: ErrorMatcher*): Unit = {

    val allEncounteredErrors = ListBuffer.empty[CompilationError]
    val remainingErrorMatchers = ListBuffer.from(errorsMatchers)

    val errorsConsumer: ErrorsConsumer = {
      case _: String => ()
      case err: CompilationError =>
        allEncounteredErrors.addOne(err)
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
        remainingErrorMatchers.map(_.details).mkString("no error found that matches the following matchers: \n", "\n", "")
          ++ "\n\nThe following errors were reported by typechecking:\n"
          ++ allEncounteredErrors.mkString("\n") ++ "\n"
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
