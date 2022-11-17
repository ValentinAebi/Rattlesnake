package compiler.parser

import compiler.Errors.{ErrorReporter, Fatal}
import compiler.irs.Asts.*
import compiler.irs.Tokens.*
import compiler.parser.ParseTree.^:
import compiler.parser.TreeParsers.{AnyTreeParser, FinalTreeParser, opt, recursive, repeat, repeatNonZero, repeatWithEnd, repeatWithSep, treeParser}
import compiler.{CompilationStep, CompilerStep, Errors, Position}
import lang.Keyword.*
import lang.Operator.*
import lang.Types.{ArrayType, StructType, Type}
import lang.{Keyword, Operator, Operators, Types}

import scala.util.Try

final class Parser(errorReporter: ErrorReporter) extends CompilerStep[(List[PositionedToken], String), Source] {
  private implicit val implErrorReporter: ErrorReporter = errorReporter

  private type P[X] = AnyTreeParser[X]

  // ---------- Syntax primitives -----------------------------------------------------------------------

  private def op(operators: Operator*) = treeParser(s"operator(s) ${operators.mkString("'", "', '", "'")}") {
    case OperatorToken(operator) if operators.contains(operator) => operator
  }

  private def kw(keywords: Keyword*) = treeParser(s"keyword(s) ${keywords.mkString("'", "', '", "'")}") {
    case KeywordToken(keyword) if keywords.contains(keyword) => keyword
  }

  private val lowName = treeParser("identifier starting with a lowercase") {
    case FirstLowercaseIdentifierToken(strValue) => strValue
  }

  private val highName = treeParser("identifier starting with an uppercase") {
    case FirstUppercaseIdentifierToken(strValue) => strValue
  }

  private val literalValue: FinalTreeParser[Literal] = treeParser("literal value") {
    case IntLitToken(value) => IntLit(value)
    case DoubleLitToken(value) => DoubleLit(value)
    case BoolLitToken(value) => BoolLit(value)
    case CharLitToken(value) => CharLit(value)
    case StringLitToken(value) => StringLit(value)
  }

  private val endOfFile = treeParser("end of file") {
    case EndOfFileToken => ()
  }

  private val assig = op(Assig).ignored
  private val doubleEqual = op(Equality).ignored
  private val openParenth = op(OpeningParenthesis).ignored
  private val closeParenth = op(ClosingParenthesis).ignored
  private val openBrace = op(OpeningBrace).ignored
  private val closeBrace = op(ClosingBrace).ignored
  private val openingBracket = op(OpeningBracket).ignored
  private val closingBracket = op(ClosingBracket).ignored
  private val comma = op(Comma).ignored
  private val dot = op(Dot).ignored
  private val colon = op(Colon).ignored
  private val doubleColon = colon ::: colon
  private val -> = (op(Minus) ::: op(GreaterThan)).ignored
  private val `=>` = (op(Assig) ::: op(GreaterThan)).ignored
  private val lessThan = op(LessThan).ignored
  private val greaterThan = op(GreaterThan).ignored

  private val unaryOperator = op(Minus, ExclamationMark, Sharp)
  private val assignmentOperator = op(PlusEq, MinusEq, TimesEq, DivEq, ModuloEq, Assig)

  private val endl = treeParser("<endl>") {
    case EndlToken => ()
  }

  private val semicolon = op(Semicolon).ignored

  // ---------- Syntax description -----------------------------------------------------------------------

  private lazy val source: FinalTreeParser[Source] = {
    repeat(topLevelDef ::: opt(op(Semicolon)).ignored) ::: endOfFile.ignored map {
      defs => Source(defs)
    }
  } setName "source"

  private lazy val topLevelDef: P[TopLevelDef] = funDef OR structDef

  private lazy val funDef = {
    kw(Fn).ignored ::: lowName ::: openParenth ::: repeatWithSep(param, comma) ::: closeParenth ::: opt(-> ::: tpe) ::: block map {
      case funName ^: params ^: optRetType ^: body =>
        FunDef(funName, params, optRetType, body)
    }
  } setName "funDef"

  private lazy val param = {
    lowName ::: colon ::: tpe map {
      case name ^: tpe =>
        Param(name, tpe)
    }
  } setName "param"

  private lazy val structDef = {
    kw(Struct).ignored ::: highName ::: openBrace ::: repeatWithSep(param, comma) ::: closeBrace map {
      case name ^: fields => StructDef(name, fields)
    }
  } setName "structDef"

  private lazy val noParenthType = recursive {
    atomicType OR arrayType
  } setName "noParenthType"

  private lazy val tpe: P[Type] = recursive {
    noParenthType OR (openParenth ::: tpe ::: closeParenth)
  } setName "tpe"

  private lazy val atomicType = {
    highName map (name => Types.primTypeFor(name).getOrElse(StructType(name)))
  } setName "atomicType"

  private lazy val arrayType = recursive {
    kw(Arr).ignored ::: tpe map (ArrayType.apply)
  } setName "arrayType"

  private lazy val block = recursive {
    openBrace ::: opt(endl).ignored ::: repeatWithEnd(stat, semicolon) ::: closeBrace map {
      stats => Block(stats)
    }
  } setName "block"

  private lazy val exprOrAssig = recursive {
    expr ::: opt(assignmentOperator ::: expr) map {
      case singleExpr ^: None => singleExpr
      case lhs ^: Some(Assig ^: rhs) => VarAssig(lhs, rhs)
      case lhs ^: Some(op ^: rhs) => VarModif(lhs, rhs, Operators.assigOperators.apply(op))
    }
  } setName "exprOrAssig"

  private lazy val assignmentStat = recursive {
    expr ::: assignmentOperator ::: expr map {
      case lhs ^: Assig ^: rhs => VarAssig(lhs, rhs)
      case lhs ^: operator ^: rhs => VarModif(lhs, rhs, Operators.assigOperators.apply(operator))
    }
  } setName "assignmentStat"

  private lazy val expr: P[Expr] = recursive {
    BinaryOperatorsParser.buildFrom(Operator.operatorsByPriorityDecreasing, noBinopExpr) ::: opt(kw(As).ignored ::: tpe) map {
      case expression ^: None => expression
      case expression ^: Some(tp) => Cast(expression, tp)
    }
  } setName "expr"

  private lazy val noBinopExpr = recursive {
    opt(unaryOperator) ::: selectOrCallChain map {
      case Some(unOp) ^: operand => UnaryOp(unOp, operand)
      case None ^: simpleExpr => simpleExpr
    }
  } setName "noBinopExpr"

  private lazy val callArgs = recursive {
    openParenth ::: repeatWithSep(expr, comma) ::: closeParenth
  } setName "callArgs"

  private lazy val indexing = recursive {
    openingBracket ::: expr ::: closingBracket
  } setName "indexing"

  private lazy val varRef = lowName map VariableRef.apply setName "varRef"

  private lazy val selectOrCallChain = recursive {
    (varRef OR literalValue OR arrayInit OR filledArrayInit OR structInit OR parenthesizedExpr OR ternary
      ) ::: repeat((dot ::: lowName) OR callArgs OR indexing) map {
      case callee ^: ls =>
        ls.foldLeft[Expr](callee) { (acc, curr) =>
          curr match {
            case field: String => Select(acc, field)
            case args: List[Expr] => Call(acc, args)
            case index: Expr => Indexing(acc, index)
          }
        }
    }
  } setName "selectOrCallChain"

  private lazy val parenthesizedExpr = recursive {
    openParenth ::: expr ::: closeParenth
  } setName "parenthesizedExpr"

  private lazy val arrayInit = recursive {
    kw(Arr).ignored ::: tpe ::: openingBracket ::: expr ::: closingBracket map {
      case elemType ^: size => ArrayInit(elemType, size)
    }
  } setName "arrayInit"

  private lazy val filledArrayInit = recursive {
    openingBracket ::: repeatWithSep(expr, comma) ::: closingBracket map (arrElems => FilledArrayInit(arrElems))
  } setName "filledArrayInit"

  private lazy val structInit = recursive {
    kw(New).ignored ::: highName ::: openBrace ::: repeatWithSep(expr, comma) ::: closeBrace map {
      case structName ^: args => StructInit(structName, args)
    }
  } setName "structInit"

  private lazy val stat: P[Statement] = {
    exprOrAssig OR valDef OR varDef OR whileLoop OR forLoop OR ifThenElse OR returnStat OR panicStat
  } setName "stat"

  private lazy val valDef = {
    kw(Val).ignored ::: lowName ::: opt(colon ::: tpe) ::: assig ::: expr map {
      case valName ^: optType ^: rhs => LocalDef(valName, optType, rhs, isReassignable = false)
    }
  } setName "valDef"

  private lazy val varDef = {
    kw(Var).ignored ::: lowName ::: opt(colon ::: tpe) ::: assig ::: expr map {
      case varName ^: optType ^: rhs => LocalDef(varName, optType, rhs, isReassignable = true)
    }
  } setName "varDef"

  private lazy val whileLoop = recursive {
    kw(While).ignored ::: expr ::: block map {
      case cond ^: body => WhileLoop(cond, body)
    }
  } setName "whileLoop"

  private lazy val forLoop = recursive {
    kw(For).ignored ::: repeatWithSep(valDef OR varDef OR assignmentStat, comma) ::: semicolon ::: expr ::: semicolon ::: repeatWithSep(assignmentStat, comma) ::: block map {
      case initStats ^: cond ^: stepStats ^: body => ForLoop(initStats, cond, stepStats, body)
    }
  } setName "forLoop"

  private lazy val ifThenElse: P[IfThenElse] = recursive {
    kw(If).ignored ::: expr ::: block ::: opt(kw(Else).ignored ::: (ifThenElse OR block)) map {
      case cond ^: thenBr ^: optElse => IfThenElse(cond, thenBr, optElse)
    }
  } setName "ifThenElse"

  private lazy val ternary = recursive {
    kw(When).ignored ::: expr ::: kw(Then).ignored ::: expr ::: kw(Else).ignored ::: expr map {
      case cond ^: thenBr ^: elseBr => Ternary(cond, thenBr, elseBr)
    }
  }

  private lazy val returnStat = {
    kw(Return).ignored ::: opt(expr) map (optRetVal => ReturnStat(optRetVal))
  } setName "returnStat"

  private lazy val panicStat = {
    kw(Panic).ignored ::: expr map PanicStat.apply
  } setName "panicStat"


  override def apply(input: (List[PositionedToken], String)): Source = {
    val (positionedTokens, srcName) = input
    if (positionedTokens.isEmpty) {
      errorReporter.pushFatal(Fatal(CompilationStep.Parsing, "empty source", Some(Position(srcName, 1, 1))))
    } else {
      val iterator = LL1Iterator.from(positionedTokens)
      source.extract(iterator) match {
        case Some(source) => source.setName(srcName)
        case None => errorReporter.displayErrorsAndTerminate()
      }
    }
  }

}
