package compiler.typechecker

import compiler.CompilationStep.TypeChecking
import compiler.Errors.{CompilationError, Err, ErrorReporter, Warning}
import compiler.irs.Asts.*
import compiler.typechecker.TypeCheckingContext.LocalInfo
import compiler.{AnalysisContext, CompilerStep, Position}
import identifiers.FunOrVarId
import lang.Operator.{Equality, Inequality, Sharp}
import lang.{Keyword, Operators, TypeConversion}
import lang.Types.*
import lang.Types.PrimitiveType.*

final class TypeChecker(errorReporter: ErrorReporter)
  extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  // TODO remove warnings for unused args in main (or create ignored modifier for parameters)

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, analysisContext) = input
    for src <- sources do {
      check(src, TypeCheckingContext(analysisContext))
    }
    errorReporter.displayAndTerminateIfErrors()
    sources.foreach(_.assertAllTypesAreSet())
    input
  }

  private def check(ast: Ast, ctx: TypeCheckingContext): Type = {
    val tpe: Type = ast match {

      case Source(defs) =>
        for df <- defs do {
          check(df, ctx.copyWithoutLocals)
        }
        VoidType

      case Block(stats) =>
        val newCtx = ctx.copied
        for stat <- stats do {
          check(stat, newCtx)
        }
        newCtx.writeLocalsRelatedWarnings(errorReporter)
        VoidType

      case StructDef(_, fields) =>
        for param@Param(_, tpe) <- fields if !ctx.knowsType(tpe) do {
          reportError(s"unknown type: $tpe", param.getPosition)
        }
        VoidType

      case funDef@FunDef(funName, params, optRetType, body) =>
        optRetType.foreach { retType =>
          if (!ctx.knowsType(retType)) {
            reportError(s"return type is unknown: '$retType'", funDef.getPosition)
          }
        }
        val expRetType = optRetType.getOrElse(VoidType)
        val ctxWithParams = ctx.copyWithoutLocals
        for param <- params do {
          val typeIsKnown = ctx.knowsType(param.tpe)
          if (!typeIsKnown) {
            reportError(s"unknown type: ${param.tpe}", param.getPosition)
          }
          val paramType = if typeIsKnown then param.tpe else UndefinedType
          ctxWithParams.addLocal(param.paramName, paramType, param.getPosition, isReassignable = false, declHasTypeAnnot = true,
            duplicateVarCallback = { () =>
              reportError(s"identifier '${param.paramName}' is already used by another parameter of function '$funName'", param.getPosition)
            }, forbiddenTypeCallback = { () =>
              reportError(s"parameter '${param.paramName}' of function '$funName' has type '$paramType', which is forbidden", param.getPosition)
            })
        }
        check(body, ctxWithParams)
        val endStatus = checkReturns(body)
        if (!endStatus.alwaysStopped && !expRetType.subtypeOf(VoidType)) {
          reportError("missing return in non-Void function", funDef.getPosition)
        }
        val faultyTypes = endStatus.returned.filter(!_.subtypeOf(expRetType))
        if (faultyTypes.nonEmpty) {
          val faultyTypeStr = faultyTypes.map("'" + _ + "'").mkString(", ")
          reportError(s"function '$funName' should return '$expRetType', found $faultyTypeStr", funDef.getPosition)
        }
        if (expRetType == NothingType && !endStatus.alwaysStopped) {
          reportError(s"cannot prove that function '$funName' with return type '$NothingType' cannot return", funDef.getPosition)
        }
        ctxWithParams.writeLocalsRelatedWarnings(errorReporter)
        VoidType

      case TestDef(_, body) =>
        check(body, ctx)
        VoidType

      case constDef@ConstDef(constName, tpeOpt, value) =>
        val inferredType = check(value, ctx)
        tpeOpt.foreach { tpe =>
          if (!inferredType.subtypeOf(tpe)) {
            reportError(s"constant is declared with type '$tpe' but defined as '$inferredType'", constDef.getPosition)
          }
        }
        VoidType

      case localDef@LocalDef(localName, optType, rhs, isReassignable) =>
        val inferredType = check(rhs, ctx)
        optType.foreach { expType =>
          val typeIsKnown = ctx.knowsType(expType)
          if (!typeIsKnown) {
            reportError(s"unknown type: '$expType'", localDef.getPosition)
          } else if (!inferredType.subtypeOf(expType)) {
            reportError(s"'$localName' should be of type '$expType', found '$inferredType'", localDef.getPosition)
          }
        }
        val actualType = optType.getOrElse(inferredType)
        localDef.optType = Some(actualType)
        ctx.addLocal(localName, actualType, localDef.getPosition, isReassignable, declHasTypeAnnot = optType.isDefined,
          duplicateVarCallback = { () =>
            reportError(s"'$localName' is already defined in this scope", localDef.getPosition)
          }, forbiddenTypeCallback = { () =>
            reportError(s"${localDef.keyword} '$localName' has type '$actualType', which is forbidden", localDef.getPosition)
          })
        markMutPropagation(actualType, rhs, ctx)
        VoidType

      case _: IntLit => IntType
      case _: DoubleLit => DoubleType
      case _: CharLit => CharType
      case _: BoolLit => BoolType
      case _: StringLit => StringType

      case varRef@VariableRef(name) =>
        ctx.localIsQueried(name)
        ctx.get(name) match {
          case Some(localInfo: LocalInfo) => localInfo.tpe
          case None =>
            reportError(s"not found: '$name'", varRef.getPosition)
        }

      case call@Call(callee, args) =>
        callee match {
          case varRef@VariableRef(name) =>
            ctx.functions.get(name) match {
              case Some(funSig) =>
                varRef.setType(UndefinedType) // useless but o.w. the check that all expressions have a type fails
                checkCallArgs(funSig.argTypes, args, ctx, call.getPosition)
                funSig.retType

              case None => reportError(s"not found: $name", call.getPosition)
            }
          case _ => reportError("syntax error, only functions can be called", call.getPosition)
        }

      case Indexing(indexed, arg) =>
        val indexedType = check(indexed, ctx)
        val indexedError = !indexedType.isInstanceOf[ArrayType]
        if (indexedError) {
          reportError("indexed expression is not an array", indexed.getPosition)
        }
        val argType = check(arg, ctx)
        val argError = !argType.subtypeOf(IntType)
        if (argError) {
          reportError(s"indexing expression should be of type '${IntType.str}', found '$argType'", arg.getPosition)
        }
        if indexedError then UndefinedType else indexedType.asInstanceOf[ArrayType].elemType

      case arrayInit@ArrayInit(elemType, size) =>
        if (elemType == VoidType || elemType == NothingType) {
          reportError(s"array cannot have element type $elemType", arrayInit.getPosition)
        }
        val sizeType = check(size, ctx)
        if (!sizeType.subtypeOf(IntType)) {
          reportError(s"array size should be of type '${IntType.str}', found '$sizeType'", size.getPosition)
        }
        size match {
          case IntLit(value) if value < 0 =>
            reportError("array size should be nonnegative", size.getPosition)
          case _ => // do nothing
        }
        ArrayType(elemType, modifiable = true)

      case filledArrayInit@FilledArrayInit(Nil, _) =>
        reportError("cannot infer type of empty array, use 'arr <type>[<size>]' instead", filledArrayInit.getPosition)

      case filledArrayInit@FilledArrayInit(arrayElems, modifiable) =>
        val types = arrayElems.map(check(_, ctx))
        types.find(tpe => types.forall(_.subtypeOf(tpe))) match {
          case Some(inferredElemType) =>
            for arrayElem <- arrayElems do {
              markMutPropagation(inferredElemType, arrayElem, ctx)
            }
            ArrayType(inferredElemType, modifiable)
          case None =>
            arrayElems.foreach {
              case VariableRef(name) => ctx.mutIsUsed(name)   // avoid false positives
              case _ => ()
            }
            reportError("cannot infer array type", filledArrayInit.getPosition)
        }

      case structInit@StructInit(structName, args, modifiable) =>
        ctx.structs.get(structName) match {
          case Some(structSig) =>
            checkCallArgs(structSig.fields.values.toList, args, ctx, structInit.getPosition)
            StructType(structName, modifiable)
          case None => reportError(s"not found: struct '$structName'", structInit.getPosition)
        }

      case unaryOp@UnaryOp(operator, operand) =>
        // no check for unused mut because no unary operator requires mutability on its argument
        val operandType = check(operand, ctx)
        if (operator == Sharp) {
          if (operandType.isInstanceOf[ArrayType] || operandType == StringType) {
            IntType
          } else {
            reportError("operator # can only be applied to arrays and strings", unaryOp.getPosition)
          }
        } else {
          Operators.unaryOperatorSignatureFor(operator, operandType) match {
            case Some(sig) => sig.retType
            case None =>
              reportError(s"no definition of operator '$operator' found for operand '$operandType'", unaryOp.getPosition)
          }
        }

      case binOp@BinaryOp(lhs, operator, rhs) =>
        // no check for unused mut because no binary operator requires mutability on its arguments
        val lhsType = check(lhs, ctx)
        val rhsType = check(rhs, ctx)
        if (operator == Equality || operator == Inequality) {
          if (lhsType != rhsType) {
            reportError(s"cannot compare '$lhsType' and '$rhsType' using ${Equality.str} or ${Inequality.str}", binOp.getPosition)
          }
          BoolType
        } else {
          Operators.binaryOperatorSigFor(lhsType, operator, rhsType) match {
            case Some(sig) => sig.retType
            case None =>
              reportError(s"no definition of operator '$operator' found for operands '$lhsType' and '$rhsType'", binOp.getPosition)
          }
        }

      case select@Select(lhs, selected) =>
        val lhsType = check(lhs, ctx)
        lhsType match {
          case StructType(typeName, structIsModifiable) =>
            val structSig = ctx.structs.apply(typeName)
            structSig.fields.get(selected) match {
              case Some(fieldType) => fieldType
              case None => reportError(s"no '$selected' field found for type '$lhsType'", select.getPosition)
            }
          case _ => reportError(s"no '$selected' field found: not a struct", select.getPosition)
        }

      case varAssig@VarAssig(lhs, rhs) =>
        val rhsType = check(rhs, ctx)
        lhs match {
          case VariableRef(name) =>
            ctx.varIsAssigned(name)
            ctx.get(name) match {
              case Some(LocalInfo(_, tpe, isReassignable, _, _)) if isReassignable =>
                lhs.setType(tpe)
                if (!rhsType.subtypeOf(tpe)) {
                  reportError(s"cannot assign a value of type '$rhsType' to a variable of type $tpe", varAssig.getPosition)
                }
              case Some(_) =>
                reportError(s"cannot mutate '$name': not a var", varAssig.getPosition)
              case None =>
                reportError(s"not found: '$name'", varAssig.getPosition)
            }
          case indexing@Indexing(indexed, _) =>
            ctx.mutIsUsed(indexing)
            val lhsType = check(indexing, ctx)
            if (indexed.getType.isInstanceOf[ArrayType] && !indexed.getType.isModifiable) {
              reportError("cannot modify an unmodifiable array", indexing.getPosition)
            }
            if (!rhsType.subtypeOf(lhsType)) {
              reportError(s"cannot assign a value of type '$rhsType' to an array of element type '$lhsType'", indexing.getPosition)
            }
          case select@Select(lhs, _) =>
            ctx.mutIsUsed(select)
            val lhsType = check(select, ctx)
            if (lhs.getType.isInstanceOf[StructType] && !lhs.getType.isModifiable) {
              reportError("cannot modify an unmodifiable struct", select.getPosition)
            }
            if (!rhsType.subtypeOf(lhsType)) {
              reportError(s"cannot assign a value of type '$rhsType' to a field of type '$lhsType'", select.getPosition)
            }
          case _ =>
            reportError("syntax error: only variables, struct fields and array elements can be assigned", varAssig.getPosition)
        }
        markMutPropagation(lhs.getType, rhs, ctx)
        VoidType

      case varModif@VarModif(lhs, rhs, op) =>
        // no check for unused mut because no operator combinable with = can operate on mutable types
        val rhsType = check(rhs, ctx)
        lhs match {
          case VariableRef(name) =>
            ctx.varIsAssigned(name)
            ctx.get(name) match {
              case Some(LocalInfo(_, tpe, isReassignable, _, _)) if isReassignable =>
                Operators.binaryOperatorSigFor(tpe, op, rhsType) match {
                  case Some(opSig) =>
                    lhs.setType(tpe)
                    if (!opSig.retType.subtypeOf(tpe)) {
                      reportError(s"$tpe ${op.str} $rhsType ==> ${opSig.retType}, not ${op.str}", varModif.getPosition)
                    }
                  case None =>
                    reportError(s"no definition of operator '$op' found for operands '$tpe' and '$rhsType'", varModif.getPosition)
                }
              case Some(_) =>
                reportError(s"cannot mutate '$name': not a var", varModif.getPosition)
              case None =>
                reportError(s"not found: '$name'", varModif.getPosition)
            }
          case indexingOrSelect: (Indexing | Select) =>
            ctx.mutIsUsed(indexingOrSelect)
            val lhsType = check(indexingOrSelect, ctx)
            indexingOrSelect match {
              case Indexing(indexed, _) if indexed.getType.isInstanceOf[ArrayType] && !indexed.getType.isModifiable =>
                reportError("cannot modify an unmodifiable array", indexingOrSelect.getPosition)
              case Select(lhs, _) if lhs.getType.isInstanceOf[StructType] && !lhs.getType.isModifiable =>
                reportError("cannot modify an unmodifiable struct", lhs.getPosition)
              case _ => ()
            }
            Operators.binaryOperatorSigFor(lhsType, op, rhsType) match {
              case Some(opSig) =>
                if (!opSig.retType.subtypeOf(lhsType)) {
                  reportError(s"$lhsType ${op.str} $rhsType ==> ${opSig.retType}, not ${op.str}", varModif.getPosition)
                }
              case None =>
                reportError(s"no definition of operator '$op' found for operands '$lhsType' and '$rhsType'", varModif.getPosition)
            }
          case _ =>
            reportError("syntax error: only variables and array elements can be assigned", varModif.getPosition)
        }
        VoidType

      case ifThenElse@IfThenElse(cond, thenBr, elseBrOpt) =>
        val condType = check(cond, ctx)
        if (!condType.subtypeOf(BoolType)) {
          reportError(s"condition should be of type '${BoolType.str}', found '$condType'", ifThenElse.getPosition)
        }
        check(thenBr, ctx)
        elseBrOpt.foreach(check(_, ctx))
        VoidType

      case ternary@Ternary(cond, thenBr, elseBr) =>
        val condType = check(cond, ctx)
        if (!condType.subtypeOf(BoolType)) {
          reportError(s"condition should be of type '${BoolType.str}', found '$condType'", ternary.getPosition)
        }
        val thenType = check(thenBr, ctx)
        val elseType = check(elseBr, ctx)
        val thenIsSupertype = elseType.subtypeOf(thenType)
        val thenIsSubtype = thenType.subtypeOf(elseType)
        val ternaryType = {
          if (thenIsSupertype) {
            thenType
          } else if (thenIsSubtype) {
            elseType
          } else {
            reportError(s"type mismatch in ternary operator: first branch has type $thenType, second has type $elseType", ternary.getPosition)
          }
        }
        markMutPropagation(ternaryType, thenBr, ctx)
        markMutPropagation(ternaryType, elseBr, ctx)
        ternaryType

      case whileLoop@WhileLoop(cond, body) =>
        val condType = check(cond, ctx)
        if (!condType.subtypeOf(BoolType)) {
          reportError(s"condition should be of type '${BoolType.str}', found '$condType'", whileLoop.getPosition)
        }
        check(body, ctx)
        VoidType

      case forLoop@ForLoop(initStats, cond, stepStats, body) =>
        val newCtx = ctx.copied
        initStats.foreach(check(_, newCtx))
        val condType = check(cond, newCtx)
        if (!condType.subtypeOf(BoolType)) {
          reportError(s"condition should be of type '${BoolType.str}', found '$condType'", forLoop.getPosition)
        }
        stepStats.foreach(check(_, newCtx))
        check(body, newCtx)
        newCtx.writeLocalsRelatedWarnings(errorReporter)
        VoidType

      case ReturnStat(valueOpt) =>
        valueOpt.foreach { value =>
          check(value, ctx)
          value match {
            case VariableRef(name) =>
              ctx.mutIsUsed(name) // to avoid false positive   TODO actual check that mut is used
            case _ => ()
          }
        }
        VoidType

      case cast@Cast(expr, tpe) =>
        val exprTpe = check(expr, ctx)
        if (exprTpe.subtypeOf(tpe)) {
          reportError(s"useless conversion: '$exprTpe' --> '$tpe'", cast.getPosition, isWarning = true)
          tpe
        } else if (TypeConversion.conversionFor(exprTpe, tpe).isDefined) {
          tpe
        } else {
          reportError(s"cannot cast ${expr.getType} to $tpe", cast.getPosition)
        }

      case panicStat@PanicStat(msg) =>
        val msgType = check(msg, ctx)
        if (!msgType.subtypeOf(StringType)) {
          reportError(s"panic can only be applied to type '${StringType.str}', found '$msgType'", panicStat.getPosition)
        }
        NothingType

      case _: (Param | Sequence) => assert(false)
    }
    // if expression save type
    ast match {
      case expr: Expr => expr.setType(tpe)
      case _ => ()
    }
    tpe
  }

  private def checkCallArgs(expTypes: List[Type], args: List[Expr], ctx: TypeCheckingContext, callPos: Option[Position]): Unit = {
    val expTypesIter = expTypes.iterator
    val argsIter = args.iterator
    var errorFound = false
    while (expTypesIter.hasNext && argsIter.hasNext && !errorFound) {
      val expType = expTypesIter.next()
      val arg = argsIter.next()
      val actType = check(arg, ctx)
      markMutPropagation(expType, arg, ctx)
      if (!actType.subtypeOf(expType)) {
        reportError(s"expected '$expType', found '$actType'", arg.getPosition)
        errorFound = true
        args.foreach {
          case VariableRef(name) => ctx.mutIsUsed(name)  // minimize false positives
          case _ => ()
        }
      }
    }
    if (expTypesIter.hasNext && !errorFound) {
      reportError(s"expected argument of type '${expTypesIter.next()}', found end of arguments list", callPos)
    }
    if (argsIter.hasNext && !errorFound) {
      val arg = argsIter.next()
      reportError(s"expected end of arguments list, found argument of type '${check(arg, ctx)}'", arg.getPosition)
      for arg <- argsIter do {
        check(arg, ctx)
      }
    }
  }

  /**
   * @param returned      types of all the expressions found after a `return`
   * @param alwaysStopped indicates whether the control-flow can reach the end of the considered construct without
   *                      encountering an instruction that terminates the function (`return` or `panic`)
   */
  private case class EndStatus(returned: Set[Type], alwaysStopped: Boolean)

  /**
   * Traverses the program, looking for instructions that end the function they are in (`return` and `panic`)
   */
  private def checkReturns(ast: Ast): EndStatus = {
    ast match {

      case Source(defs) =>
        for df <- defs do {
          checkReturns(df)
        }
        EndStatus(Set.empty, false)

      case Block(stats) =>
        var endStatus = EndStatus(Set.empty, false)
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
            if (thenEndStatus.returned.size == 1 && elseEndStatus.returned.size == 1) {
              val thenRet = thenEndStatus.returned.head
              val elseRet = elseEndStatus.returned.head
              if (!thenRet.subtypeOrSupertype(elseRet)) {
                reportError(s"branches of if lead to different return types: '$thenRet', '$elseRet'", ifThenElse.getPosition)
              }
            }
            EndStatus(thenEndStatus.returned ++ elseEndStatus.returned, thenEndStatus.alwaysStopped && elseEndStatus.alwaysStopped)
          case None => thenEndStatus.copy(alwaysStopped = false)
        }

      case _: Ternary => EndStatus(Set.empty, false)

      case whileLoop@WhileLoop(_, body) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("while should be replaced by if", whileLoop.getPosition, isWarning = true)
        }
        EndStatus(bodyEndStatus.returned, false)

      case forLoop@ForLoop(_, _, _, body) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("for should be replaced by if", forLoop.getPosition, isWarning = true)
        }
        EndStatus(bodyEndStatus.returned, false)

      case retStat: ReturnStat =>
        val retType = retStat.getRetType
        if (retType.isEmpty) {
          reportError("could not infer type of returned value", retStat.getPosition)
        }
        EndStatus(Set(retType.getOrElse(VoidType)), true)

      case _: PanicStat =>
        EndStatus(Set(NothingType), true)

      case _: (FunDef | StructDef | Param) =>
        assert(false)

      case _ => EndStatus(Set.empty, false)

    }
  }

  private def reportError(msg: String, pos: Option[Position], isWarning: Boolean = false): UndefinedType.type = {
    errorReporter.push(if isWarning then Warning(TypeChecking, msg, pos) else Err(TypeChecking, msg, pos))
    UndefinedType
  }

  private def markMutPropagation(assignedLocalName: FunOrVarId, rhs: Expr, ctx: TypeCheckingContext): Unit = {
    rhs match
      case VariableRef(name) if ctx.get(assignedLocalName).exists(_.tpe.isModifiable) =>
        ctx.mutIsUsed(name)
      case _ => ()
  }

  private def markMutPropagation(expectedType: Type, rhs: Expr, ctx: TypeCheckingContext): Unit = {
    rhs match
      case VariableRef(name) if expectedType.isModifiable =>
        ctx.mutIsUsed(name)
      case _ => ()
  }

}
