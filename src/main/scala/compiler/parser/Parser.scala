package compiler.parser

import compiler.pipeline.CompilationStep.Parsing
import compiler.reporting.Errors.{Err, ErrorReporter, Fatal}
import compiler.irs.Asts.*
import compiler.irs.Tokens.*
import compiler.parser.ParseTree.^:
import compiler.parser.TreeParsers.*
import compiler.pipeline.CompilerStep
import compiler.reporting.{Errors, Position}
import identifiers.*
import lang.Captures.*
import lang.Keyword.*
import lang.Operator.*
import lang.Types.PrimitiveType.RegionType
import lang.Types.{ArrayType, NamedType, Type}
import lang.{Keyword, Operator, Operators, Types}

import scala.compiletime.uninitialized

final class Parser(errorReporter: ErrorReporter) extends CompilerStep[(List[PositionedToken], String), Source] {
  private implicit val implErrorReporter: ErrorReporter = errorReporter
  private var ll1Iterator: LL1Iterator = uninitialized

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
  private val maybeSemicolon = opt(op(Semicolon)).ignored
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

  private lazy val topLevelDef: P[TopLevelDef] = moduleDef OR packageDef OR structDef OR constDef

  private lazy val packageDef: P[PackageDef] = {
    kw(Package).ignored ::: highName ::: openBrace ::: repeat(funDef ::: maybeSemicolon) ::: closeBrace map {
      case packageName ^: functions =>
        PackageDef(packageName, functions)
    }
  } setName "packageDef"

  private lazy val moduleDef: P[ModuleDef] = {
    kw(Module).ignored ::: highName
      ::: openParenth ::: repeatWithSep(importTree, comma) ::: closeParenth
      ::: openBrace ::: repeat(funDef ::: maybeSemicolon) ::: closeBrace map {
      case moduleName ^: imports ^: functions =>
        ModuleDef(moduleName, imports, functions)
    }
  } setName "moduleDef"

  private lazy val funDef = {
    kw(Fn).ignored ::: lowName ::: openParenth ::: repeatWithSep(param, comma) ::: closeParenth ::: opt(-> ::: tpe) ::: block map {
      case funName ^: params ^: optRetType ^: body =>
        FunDef(funName, params, optRetType, body)
    }
  } setName "funDef"

  private lazy val moduleImport = lowName ::: colon ::: tpe map {
    case paramId ^: paramType => ParamImport(paramId, paramType)
  } setName "moduleImport"

  private lazy val packageImport = {
    kw(Package).ignored ::: highName map (PackageImport(_))
  } setName "packageImport"

  private lazy val deviceImport = kw(Device).ignored ::: device map {
    device => DeviceImport(device)
  } setName "deviceImport"

  private lazy val importTree = moduleImport OR packageImport OR deviceImport

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
    noParenthType OR (openParenth ::: tpe ::: closeParenth)
  } setName "tpe"

  private lazy val atomicType = highName ::: opt(op(Hat) ::: opt(captureDescr)) map {
    case shape ^: capDescrTokensOpt => {
      val capDescr = capDescrTokensOpt.map {
        case hat ^: descrOpt => descrOpt.getOrElse(CaptureSet.singletonOfRoot)
      }.getOrElse(CaptureSet.empty)
      val primTypeOpt = Types.primTypeFor(shape)
      if (primTypeOpt.isDefined && capDescrTokensOpt.isDefined){
        errorReporter.push(Err(Parsing,
          s"primitive type $shape is not allowed to specify a capture set (the capture set of $RegionType is implicit)",
          ll1Iterator.current.position
        ))
      }
      primTypeOpt.getOrElse(NamedType(shape, capDescr))
    }
  } setName "atomicType"

  private lazy val properPath: P[ProperPath] = lowName ::: repeat(dot ::: lowName) map {
    case root ^: selects => selects.foldLeft[ProperPath](VarPath(root))(SelectPath(_, _))
  } setName "properPath"

  private lazy val captureSet = openBrace ::: repeatWithSep(properPath, comma) ::: closeBrace map {
    case paths => CaptureSet(paths.toSet)
  } setName "captureSet"

  private lazy val brand = {
    op(Sharp) map (_ => Brand)
  } setName "brand"

  private lazy val captureDescr: P[CaptureDescriptor] = {
    captureSet OR brand
  } setName "captureDescr"

  private lazy val arrayType = recursive {
    opt(kw(Mut)) ::: kw(Arr).ignored ::: tpe map {
      case mutOpt ^: tp => ArrayType(tp, mutOpt.isDefined)
    }
  } setName "arrayType"

  private lazy val device = kw(lang.Device.kwToDevice.keys.toSeq *) map {
    deviceKw => lang.Device.kwToDevice.apply(deviceKw)
  } setName "device"

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
    BinaryOperatorsParser.buildFrom(Operator.operatorsByPriorityDecreasing, binopArg)
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
    (noBinopExpr OR mutPossiblyFilledArrayInit OR arrayInit OR structOrModuleInstantiation) ::: opt((kw(As) OR kw(Is)) ::: tpe) map {
      case expression ^: None => expression
      case expression ^: Some(As ^: tp) => Cast(expression, tp)
      case expression ^: Some(Is ^: tp) => TypeTest(expression, tp)
      case _ => assert(false)
    }
  } setName "binopArg"

  private lazy val callArgs = recursive {
    openParenth ::: repeatWithSep(expr, comma) ::: closeParenth
  } setName "callArgs"

  private lazy val indexing = recursive {
    openingBracket ::: expr ::: closingBracket
  } setName "indexing"

  private lazy val me = kw(Me) map (_ => MeRef())

  private lazy val pkgRef = highName map (PackageRef(_))

  private lazy val deviceRef = device map (DeviceRef(_))

  private lazy val varRefOrIntrinsicCall = lowName ::: opt(callArgs) map {
    case name ^: Some(args) => Call(None, name, args)
    case name ^: None => VariableRef(name)
  } setName "varRefOrIntrinsicCall"

  private lazy val atomicExpr = recursive {
    varRefOrIntrinsicCall OR me OR pkgRef OR literalValue OR filledArrayInit OR parenthesizedExpr
  } setName "atomicExpr"

  private lazy val selectOrIndexingChain = recursive {
    atomicExpr ::: repeat((dot ::: lowName ::: opt(callArgs)) OR indexing) map {
      case atExpr ^: repeated =>
        repeated.foldLeft(atExpr) {
          case (acc, name ^: Some(args)) => Call(Some(acc), name, args)
          case (acc, name ^: None) => Select(acc, name)
          case (acc, index: Expr) => Indexing(acc, index)
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

  private lazy val structOrModuleInstantiation = recursive {
    kw(New).ignored ::: highName ::: openParenth ::: repeatWithSep(expr, comma) ::: closeParenth map {
      case tid ^: args => StructOrModuleInstantiation(tid, args)
    }
  } setName "structOrModuleInstantiation"

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
