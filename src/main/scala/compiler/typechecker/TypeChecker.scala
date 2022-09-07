package compiler.typechecker

import compiler.CompilationStep.TypeChecking
import compiler.Errors.{CompilationError, ErrorReporter}
import compiler.irs.Asts.*
import compiler.{CompilerStep, Position}
import lang.Operator.{Equality, Inequality}
import lang.Operators
import lang.Types.*
import lang.Types.PrimitiveType.*

final class TypeChecker(errorReporter: ErrorReporter) extends CompilerStep[List[Source], List[Source]] {

  override def apply(sources: List[Source]): List[Source] = {
    val ctx = buildContext(sources)
    for src <- sources do {
      check(src, ctx.copyWithoutLocals)
    }
    errorReporter.displayAndTerminateIfErrors()
    sources
  }

  private def buildContext(sources: List[Source]): AnalysisContext = {
    val ctxBuilder = new AnalysisContext.Builder(errorReporter)
    for src <- sources do {
      for df <- src.defs do {
        df match
          case funDef: FunDef =>
            ctxBuilder.addFunction(funDef)
          case structDef: StructDef =>
            ctxBuilder.addStruct(structDef)
      }
    }
    ctxBuilder.built
  }

  private def check(ast: Ast, ctx: AnalysisContext): Type = {
    ast match {

      case Source(defs) =>
        for df <- defs if df.isInstanceOf[FunDef] do {
          check(df, ctx.copyWithoutLocals)
        }
        VoidType

      case Block(stats) =>
        val newCtx = ctx.copied
        for stat <- stats do {
          check(stat, newCtx)
        }
        VoidType

      case funDef@FunDef(funName, params, optRetType, body) =>
        val expRetType = optRetType.getOrElse(VoidType)
        val ctxWithParams = ctx.copyWithoutLocals
        for param <- params do {
          ctxWithParams.addLocal(param.paramName, param.tpe, false)
        }
        check(body, ctxWithParams)
        val endStatus = checkReturns(body)
        if (!endStatus.alwaysStopped && expRetType != VoidType) {
          reportError("missing return in non-Void function", funDef.getPosition)
        }
        endStatus.returned.foreach { actRetType =>
          if (actRetType != expRetType) {
            reportError(s"function '$funName' should return '$expRetType', found '$actRetType'", funDef.getPosition)
          }
        }
        VoidType

      case valDef @ ValDef(valName, optType, rhs) =>
        val actType = check(rhs, ctx)
        optType.foreach { expType =>
          if (actType != expType) {
            reportError(s"val should be of type '$expType', found '$actType'", valDef.getPosition)
          }
        }
        ctx.addLocal(valName, optType.getOrElse(actType), false)
        VoidType

      case varDef @ VarDef(varName, optType, rhs) =>
        val actType = check(rhs, ctx)
        optType.foreach { expType =>
          if (actType != expType) {
            reportError(s"val should be of type '$expType', found '$actType'", varDef.getPosition)
          }
        }
        ctx.addLocal(varName, optType.getOrElse(actType), true)
        VoidType

      case _: IntLit => IntType
      case _: DoubleLit => DoubleType
      case _: CharLit => CharType
      case _: BoolLit => BoolType
      case _: StringLit => StringType

      case varRef@VariableRef(name) =>
        ctx.locals.get(name) match {
          case Some((tpe, _)) => tpe
          case None =>
            reportError(s"not found: '$name'", varRef.getPosition)
            VoidType
        }

      case call@Call(callee, args) =>
        callee match {
          case VariableRef(name) =>
            ctx.functions.get(name) match {
              case Some(funSig) =>
                checkCallArgs(funSig.argTypes, args, ctx, call.getPosition)
                funSig.retType

              case None =>
                reportError("syntax error, only functions can be called", call.getPosition)
                VoidType
            }
          case _ =>
            reportError("syntax error", call.getPosition)
            VoidType
        }

      case Indexing(indexed, arg) =>
        val indexedType = check(indexed, ctx)
        val indexedError = !indexedType.isInstanceOf[ArrayType]
        if (indexedError) {
          reportError("indexed expression is not an array", indexed.getPosition)
        }
        val argType = check(arg, ctx)
        val argError = argType != IntType
        if (argError) {
          reportError(s"indexing expression should be an '${IntType.str}'", arg.getPosition)
        }
        if (indexedError) VoidType else indexedType.asInstanceOf[ArrayType].elemType

      case ArrayInit(elemType, size) =>
        val sizeType = check(size, ctx)
        if (sizeType != IntType) {
          reportError(s"array size should be an '${IntType.str}'", size.getPosition)
        }
        size match {
          case IntLit(value) if value < 0 =>
            reportError("array size should be nonnegative", size.getPosition)
          case _ => // do nothing
        }
        ArrayType(elemType)

      case structInit @ StructInit(structName, args) =>
        ctx.structs.get(structName) match {
          case Some(structSig) =>
            checkCallArgs(structSig.fields.values.toList, args, ctx, structInit.getPosition)
            StructType(structName)

          case None =>
            reportError(s"not found: struct '$structName'", structInit.getPosition)
            VoidType
        }

      case unaryOp @ UnaryOp(operator, operand) =>
        val operandType = check(operand, ctx)
        Operators.unaryOpFor(operator, operandType) match {
          case Some(sig) => sig.retType
          case None =>
            reportError(s"no definition of operator '$operator' found for operand '$operandType'", unaryOp.getPosition)
            VoidType
        }

      case binOp @ BinaryOp(lhs, operator, rhs) =>
        val lhsType = check(lhs, ctx)
        val rhsType = check(rhs, ctx)
        if (operator == Equality || operator == Inequality){
          if (lhsType == rhsType){
            reportError(s"operands of '${Equality.str}' and '${Inequality.str}' should have the same type", binOp.getPosition)
          }
          BoolType
        } else {
          Operators.binaryOpFor(lhsType, operator, rhsType) match {
            case Some(sig) => sig.retType
            case None =>
              reportError(s"no definition of operator '$operator' found for operands '$lhsType' and '$rhsType'", binOp.getPosition)
              VoidType
          }
        }

      case select @ Select(lhs, selected) =>
        val lhsType = check(lhs, ctx)
        lhsType match {
          case StructType(typeName) =>
            val structSig = ctx.structs.apply(typeName)
            structSig.fields.get(typeName) match {
              case Some(fieldType) => fieldType
              case None =>
                reportError(s"no '$selected' field found for type '$lhsType'", select.getPosition)
                VoidType
            }
          case _ =>
            reportError(s"no '$selected' field found: type is not a struct", select.getPosition)
            VoidType
        }

      case varAssig @ VarAssig(lhs, rhs) =>
        val rhsType = check(rhs, ctx)
        lhs match {
          case VariableRef(name) =>
            ctx.locals.get(name) match {
              case Some((tpe, mut)) if mut =>
                if (tpe != rhsType){
                  reportError(s"cannot assign a '$rhsType' to a variable of type $tpe", varAssig.getPosition)
                }
              case Some(_) =>
                reportError(s"cannot mutate '$name': is a val, not a var", varAssig.getPosition)
              case None =>
                reportError(s"not found: '$name'", varAssig.getPosition)
            }
          case _ =>
            reportError("syntax error: only variables can be assigned", varAssig.getPosition)
        }
        VoidType

      case ifThenElse @ IfThenElse(cond, thenBr, elseBrOpt) =>
        val condType = check(cond, ctx)
        if (condType != BoolType){
          reportError(s"condition should be of type '${BoolType.str}', found '$condType'", ifThenElse.getPosition)
        }
        check(thenBr, ctx)
        elseBrOpt.foreach(check(_, ctx))
        VoidType

      case whileLoop @ WhileLoop(cond, body) =>
        val condType = check(cond, ctx)
        if (condType != BoolType) {
          reportError(s"condition should be of type '${BoolType.str}', found '$condType'", whileLoop.getPosition)
        }
        check(body, ctx)
        VoidType

      case retStat @ ReturnStat(value) =>
        val tpe = check(value, ctx)
        retStat.setRetType(tpe)
        VoidType

      case _: (StructDef | Param) => assert(false)
    }
  }

  private case class EndStatus(returned: Option[Type], alwaysStopped: Boolean)

  private def checkCallArgs(expTypes: List[Type], args: List[Expr], ctx: AnalysisContext, callPos: Option[Position]): Unit = {
    val expTypesIter = expTypes.iterator
    val argsIter = args.iterator
    var errorFound = false
    while (expTypesIter.hasNext && argsIter.hasNext && !errorFound) {
      val expType = expTypesIter.next()
      val arg = argsIter.next()
      val actType = check(arg, ctx)
      if (expType != actType) {
        reportError(s"expected $expType, found $actType", arg.getPosition)
        errorFound = true
      }
    }
    if (expTypesIter.hasNext) {
      reportError(s"expected ${expTypesIter.next()}, found end of arguments list", callPos)
    }
    if (argsIter.hasNext) {
      val arg = argsIter.next()
      reportError(s"expected end of arguments list, found ${check(arg, ctx)}", arg.getPosition)
      for arg <- argsIter do {
        check(arg, ctx)
      }
    }
  }

  private def checkReturns(ast: Ast): EndStatus = {
    ast match {

      case Source(defs) =>
        for df <- defs do {
          checkReturns(df)
        }
        EndStatus(None, false)

      case Block(stats) =>
        var endStatus = EndStatus(None, false)
        for stat <- stats do {
          if (endStatus.alwaysStopped) {
            reportError("dead code", stat.getPosition)
          } else {
            endStatus = checkReturns(stat)
          }
        }
        endStatus

      case ifThenElse@IfThenElse(_, thenBr, elseBrOpt) =>
        val thenEndStatus = checkReturns(thenBr)
        elseBrOpt match {
          case Some(elseBr) =>
            val elseEndStatus = checkReturns(elseBr)
            val alwaysStopped = thenEndStatus.alwaysStopped && elseEndStatus.alwaysStopped
            if (thenEndStatus.returned.isDefined && elseEndStatus.returned.isDefined) {
              if (thenEndStatus.returned != elseEndStatus.returned) {
                reportError("branches of if lead to different return types", ifThenElse.getPosition)
              }
              EndStatus(thenEndStatus.returned, alwaysStopped)
            } else if (thenEndStatus.returned.isDefined) {
              EndStatus(thenEndStatus.returned, alwaysStopped)
            } else {
              EndStatus(elseEndStatus.returned, alwaysStopped)
            }
          case None => EndStatus(thenEndStatus.returned, false)
        }

      case whileLoop@WhileLoop(_, body) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("while should be replaced by if", whileLoop.getPosition)
        }
        EndStatus(bodyEndStatus.returned, false)

      case retStat: ReturnStat =>
        val retType = retStat.getRetType
        if (retType.isEmpty) {
          reportError("could not infer type of returned value", retStat.getPosition)
        }
        EndStatus(retType, true)

      case _: (FunDef | StructDef | Param) =>
        assert(false)

      case _ => EndStatus(None, false)

    }
  }

  private def reportError(msg: String, pos: Option[Position]): Unit = {
    errorReporter.push(new CompilationError(TypeChecking, msg, pos))
  }

}
