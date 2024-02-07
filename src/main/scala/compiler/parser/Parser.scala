package compiler.parser

import compiler.CompilationStep.Parsing
import compiler.Errors.{ErrorReporter, Fatal, errorsExitCode}
import compiler.irs.Asts.*
import compiler.irs.Tokens.*
import compiler.parser.ParseTree.^:
import compiler.parser.TreeParsers.*
import compiler.prettyprinter.PrettyPrinter
import compiler.{CompilerStep, Errors, Position}
import identifiers.*
import lang.Keyword.*
import lang.Operator.*
import lang.Types.{ArrayType, StructType, Type}
import lang.{Keyword, Operator, Operators, Types}

import scala.util.Try

final class Parser(errorReporter: ErrorReporter) extends CompilerStep[(List[PositionedToken], String), Source] {
  private implicit val implErrorReporter: ErrorReporter = errorReporter
  private var ll1Iterator: LL1Iterator = _

  private type P[X] = AnyTreeParser[X]

  // ---------- Syntax primitives -----------------------------------------------------------------------

  private def op(operators: Operator*) = treeParser(operators.mkString(" or ")) {
    case OperatorToken(operator) if operators.contains(operator) => operator
  }

  private def kw(keywords: Keyword*) = treeParser(keywords.mkString(" or ")) {
    case KeywordToken(keyword) if keywords.contains(keyword) => keyword
  }

  private val lowName = treeParser("a..z") {
    case FirstLowercaseIdentifierToken(strValue) => NormalFunOrVarId(strValue)
  }

  private val highName = treeParser("A..Z") {
    case FirstUppercaseIdentifierToken(strValue) => NormalTypeId(strValue)
  }

  private val numericLiteralValue: FinalTreeParser[NumericLiteral] = treeParser("int or double") {
    case IntLitToken(value) => IntLit(value)
    case DoubleLitToken(value) => DoubleLit(value)
  }

  private val nonNumericLiteralValue: FinalTreeParser[NonNumericLiteral] = treeParser("bool or char or string") {
    case BoolLitToken(value) => BoolLit(value)
    case CharLitToken(value) => CharLit(value)
    case StringLitToken(value) => StringLit(value)
  }

  private val literalValue: FinalTreeParser[Literal] = {
    numericLiteralValue OR nonNumericLiteralValue
  } setName "literalValue"

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

  private lazy val topLevelDef: P[TopLevelDef] = funDef OR structDef OR testDef OR constDef

  private lazy val funDef = {
    kw(Fn).ignored ::: lowName ::: openParenth ::: repeatWithSep(param, comma) ::: closeParenth ::: opt(-> ::: tpe) ::: block map {
      case funName ^: params ^: optRetType ^: body =>
        FunDef(funName, params, optRetType, body)
    }
  } setName "funDef"

  private lazy val param = {
    opt(kw(Var)) ::: opt(lowName ::: colon) ::: tpe map {
      case optVar ^: name ^: tpe =>
        Param(name, tpe, optVar.isDefined)
    }
  } setName "param"

  private lazy val structDef = {
    (kw(Struct) OR kw(Interface)) ::: highName ::: opt(colon ::: repeatWithSep(highName, comma))
      ::: openBrace ::: repeatWithSep(param, comma) ::: closeBrace map {
      case structOrInterface ^: name ^: supertypesOpt ^: fields =>
        StructDef(name, fields, supertypesOpt.getOrElse(Seq.empty), structOrInterface == Interface)
    }
  } setName "structDef"

  private lazy val testDef = {
    kw(Test).ignored ::: lowName ::: block map {
      case name ^: body => TestDef(name, body)
    }
  } setName "testDef"

  private lazy val possiblyNegativeNumericLiteralValue: FinalTreeParser[NumericLiteral] = {
    (numericLiteralValue OR (op(Minus) ::: numericLiteralValue)) map {
      case _ ^: IntLit(value) => IntLit(-value)
      case _ ^: DoubleLit(value) => DoubleLit(-value)
      case (lit: NumericLiteral) => lit
    }
  } setName "possiblyNegativeNumericLiteralValue"

  private lazy val constExprLiteralValue = {
    possiblyNegativeNumericLiteralValue OR nonNumericLiteralValue
  } setName "constExprLiteralValue"

  private lazy val constDef = {
    kw(Const).ignored ::: lowName ::: opt(colon ::: tpe) ::: assig ::: constExprLiteralValue map {
      case name ^: tpeOpt ^: value => ConstDef(name, tpeOpt, value)
    }
  } setName "constDef"

  private lazy val noParenthType = recursive {
    atomicType OR arrayType
  } setName "noParenthType"

  private lazy val tpe: P[Type] = recursive {
    modifiableType OR unmodifiableType
  } setName "tpe"

  private lazy val modifiableType: P[Type] = recursive {
    kw(Mut).ignored ::: unmodifiableType map {
      case StructType(typeName, _) => StructType(typeName, modifiable = true)
      case ArrayType(elemType, _) => ArrayType(elemType, modifiable = true)
      case tpe =>
        val pos = ll1Iterator.current.position // imprecise (takes the position on the next token)
        errorReporter.push(Errors.Err(Parsing, s"$tpe is never modifiable", Some(pos)))
        tpe
    }
  } setName "modifiableType"

  private lazy val unmodifiableType: P[Type] = recursive {
    noParenthType OR (openParenth ::: tpe ::: closeParenth)
  } setName "unmodifiableType"

  private lazy val atomicType = {
    highName map (name => Types.primTypeFor(name).getOrElse(StructType(name, modifiable = false)))
  } setName "atomicType"

  private lazy val arrayType = recursive {
    kw(Arr).ignored ::: tpe map (ArrayType.apply(_, modifiable = false))
  } setName "arrayType"

  private lazy val block = recursive {
    openBrace ::: repeatWithEnd(stat, semicolon) ::: closeBrace map {
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
    noTernaryExpr OR ternary
  } setName "expr"

  private lazy val noTernaryExpr: P[Expr] = recursive {
    BinaryOperatorsParser.buildFrom(Operator.operatorsByPriorityDecreasing, binopArg) ::: opt(kw(As).ignored ::: tpe) map {
      case expression ^: None => expression
      case expression ^: Some(tp) => Cast(expression, tp)
    }
  } setName "noTernaryExpr"

  private lazy val noBinopExpr = recursive {
    opt(unaryOperator) ::: selectOrIndexingChain map {
      case Some(Minus) ^: IntLit(value) => IntLit(-value)
      case Some(Minus) ^: DoubleLit(value) => DoubleLit(-value)
      case Some(unOp) ^: operand => UnaryOp(unOp, operand)
      case None ^: simpleExpr => simpleExpr
    }
  } setName "noBinopExpr"

  private lazy val binopArg = recursive {
    noBinopExpr OR mutPossiblyFilledArrayInit OR arrayInit OR structInit
  } setName "binopArg"

  private lazy val callArgs = recursive {
    openParenth ::: repeatWithSep(expr, comma) ::: closeParenth
  } setName "callArgs"

  private lazy val indexing = recursive {
    openingBracket ::: expr ::: closingBracket
  } setName "indexing"

  private lazy val varRefOrFunCall = recursive {
    lowName ::: opt(callArgs) map {
      case name ^: Some(args) =>
        Call(VariableRef(name), args)
      case name ^: None =>
        VariableRef(name)
    }
  } setName "varRefOrCallArgs"

  private lazy val atomicExpr = {
    varRefOrFunCall OR literalValue OR filledArrayInit OR parenthesizedExpr
  } setName "atomicExpr"

  private lazy val selectOrIndexingChain = recursive {
    atomicExpr ::: repeat((dot ::: lowName) OR indexing) map {
      case atExpr ^: selOrInds =>
        selOrInds.foldLeft(atExpr) { (acc, curr) =>
          curr match
            case field: NormalFunOrVarId => Select(acc, field)
            case idx: Expr => Indexing(acc, idx)
        }
    }
  } setName "selectOrIndexingChain"

  private lazy val parenthesizedExpr = recursive {
    openParenth ::: expr ::: closeParenth
  } setName "parenthesizedExpr"

  private lazy val mutPossiblyFilledArrayInit = recursive {
    kw(Mut).ignored ::: (arrayInit OR filledArrayInit) map {
      case arrayInit: ArrayInit =>
        errorReporter.push(Errors.Warning(Parsing,
          s"${Mut.str} is redundant in front of an uninitialized array declaration", arrayInit.getPosition))
        arrayInit
      case filledArrayInit: FilledArrayInit =>
        filledArrayInit.copy(modifiable = true)
    }
  } setName "mutPossiblyFilledArrayInit"

  private lazy val arrayInit = recursive {
    kw(Arr).ignored ::: tpe ::: openingBracket ::: expr ::: closingBracket map {
      case elemType ^: size =>
        ArrayInit(elemType, size)
    }
  } setName "arrayInit"

  private lazy val filledArrayInit = recursive {
    openingBracket ::: repeatWithSep(expr, comma) ::: closingBracket map {
      case arrElems => FilledArrayInit(arrElems, false)
    }
  } setName "filledArrayInit"

  private lazy val structInit = recursive {
    kw(New).ignored ::: opt(kw(Mut)) ::: highName ::: openBrace ::: repeatWithSep(expr, comma) ::: closeBrace map {
      case optMut ^: structName ^: args => StructInit(structName, args, optMut.isDefined)
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
    kw(For).ignored ::: repeatWithSep(valDef OR varDef OR assignmentStat, comma) ::: semicolon
      ::: expr ::: semicolon ::: repeatWithSep(assignmentStat, comma) ::: block map {
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
  } setName "ternary"

  private lazy val returnStat = {
    kw(Return).ignored ::: opt(expr) map (optRetVal => ReturnStat(optRetVal))
  } setName "returnStat"

  private lazy val panicStat = {
    kw(Panic).ignored ::: expr map PanicStat.apply
  } setName "panicStat"


  override def apply(input: (List[PositionedToken], String)): Source = {
    val (positionedTokens, srcName) = input
    if (positionedTokens.isEmpty) {
      errorReporter.pushFatal(Fatal(Parsing, "empty source", Some(Position(srcName, 1, 1))))
    } else {
      ll1Iterator = LL1Iterator.from(positionedTokens)
      source.extract(ll1Iterator) match {
        case Some(source) => source.setName(srcName)
        case None => errorReporter.displayErrorsAndTerminate()
      }
    }
  }

}
