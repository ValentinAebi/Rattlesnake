package compiler.typechecker

import compiler.CompilationStep.TypeChecking
import compiler.Errors.{CompilationError, Err, ErrorReporter, Warning}
import compiler.irs.Asts.*
import compiler.typechecker.TypeCheckingContext.LocalInfo
import compiler.{AnalysisContext, CompilerStep, Position}
import identifiers.{FunOrVarId, TypeIdentifier}
import lang.*
import lang.Operator.{Equality, Inequality, Sharp}
import lang.StructSignature.FieldInfo
import lang.SubtypeRelation.subtypeOf
import lang.Types.*
import lang.Types.PrimitiveType.*

final class TypeChecker(errorReporter: ErrorReporter)
  extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, analysisContext) = input
    for src <- sources do {
      check(src, TypeCheckingContext(analysisContext, currentFunctionName = None))  // expectedRetType is be ignored here
    }
    errorReporter.displayAndTerminateIfErrors()
    sources.foreach(_.assertAllTypesAreSet())
    input
  }

  private def check(ast: Ast, ctx: TypeCheckingContext): Type = {
    given Map[TypeIdentifier, StructSignature] = ctx.structs
    val tpe: Type = ast match {

      case Source(defs) =>
        for df <- defs do {
          check(df, ctx)
        }
        VoidType

      case Block(stats) =>
        val newCtx = ctx.copied
        for stat <- stats do {
          check(stat, newCtx)
        }
        newCtx.writeLocalsRelatedWarnings(errorReporter)
        VoidType

      case StructDef(_, fields, _, _) =>
        for param@Param(_, tpe, _) <- fields do {
          typeMustBeKnown(tpe, ctx, param.getPosition)
        }
        VoidType

      case funDef@FunDef(funName, params, optRetType, body) =>
        optRetType.foreach { retType =>
          if (!ctx.knowsType(retType)) {
            reportError(s"return type is unknown: '$retType'", funDef.getPosition)
          }
        }
        val expRetType = optRetType.getOrElse(VoidType)
        val bodyCtx = ctx.copyWithoutLocals(funName)
        for param <- params do {
          val paramType = typeMustBeKnown(param.tpe, ctx, param.getPosition)
          param.paramNameOpt.foreach { paramName =>
            bodyCtx.addLocal(paramName, paramType, param.getPosition, param.isReassignable, declHasTypeAnnot = true,
              duplicateVarCallback = { () =>
                reportError(s"identifier '${param.paramNameOpt}' is already used by another parameter of function '$funName'", param.getPosition)
              }, forbiddenTypeCallback = { () =>
                reportError(s"parameter '${param.paramNameOpt}' of function '$funName' has type '$paramType', which is forbidden", param.getPosition)
              })
          }
        }
        check(body, bodyCtx)
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
        bodyCtx.writeLocalsRelatedWarnings(errorReporter)
        VoidType

      case TestDef(_, body) =>
        check(body, ctx)
        VoidType

      case constDef@ConstDef(constName, tpeOpt, value) =>
        val inferredType = check(value, ctx)
        tpeOpt.foreach { expType =>
          val checkedType = typeMustBeKnown(expType, ctx, constDef.getPosition)
          checkSubtypingConstraint(checkedType, inferredType, constDef.getPosition, "constant definition")
        }
        VoidType

      case localDef@LocalDef(localName, optType, rhs, isReassignable) =>
        val inferredType = check(rhs, ctx)
        optType.foreach { expType =>
          val checkedType = typeMustBeKnown(expType, ctx, localDef.getPosition)
          checkSubtypingConstraint(checkedType, inferredType, localDef.getPosition, "local definition")
        }
        val requestExplicitType = optType.isEmpty && inferredType.isInstanceOf[UnionType]
        if (requestExplicitType){
          reportError(s"Please provide an explicit type for $localName", localDef.getPosition)
        }
        val actualType = if requestExplicitType then UndefinedType else optType.getOrElse(inferredType)
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
        mustBeKnown(name, ctx, varRef.getPosition)

      case call@Call(callee, args) =>
        callee match {
          case varRef@VariableRef(name) =>
            varRef.setType(UndefinedType) // useless but o.w. the check that all expressions have a type fails
            checkFunCall(name, args, call.getPosition, ctx)
          case _ => reportError("syntax error, only functions can be called", call.getPosition)
        }

      case call@TailCall(funId, args) =>
        if (funId != ctx.currentFunctionName.get){
          reportError("tail calls can only refer to the enclosing function", call.getPosition)
        }
        if (BuiltInFunctions.builtInFunctions.contains(funId)){
          reportError("built-in functions do not support tail calls", call.getPosition)
        }
        // the fact that the call is indeed in tail position is checked in the backend
        checkFunCall(funId, args, call.getPosition, ctx)

      case indexing@Indexing(indexed, arg) =>
        val indexedType = check(indexed, ctx)
        val argType = check(arg, ctx)
        checkSubtypingConstraint(IntType, argType, indexing.getPosition, "array index")
        exprMustBeArray(indexed.getType, ctx, indexed.getPosition, mustUpdate = false)

      case arrayInit@ArrayInit(elemType, size) =>
        if (elemType == VoidType || elemType == NothingType) {
          reportError(s"array cannot have element type $elemType", arrayInit.getPosition)
        }
        val sizeType = check(size, ctx)
        checkSubtypingConstraint(IntType, sizeType, size.getPosition, "array size")
        size match {
          case IntLit(value) if value < 0 =>
            reportError("array size should be nonnegative", size.getPosition)
          case _ => ()
        }
        val checkedType = typeMustBeKnown(elemType, ctx, arrayInit.getPosition)
        ArrayType(elemType, modifiable = true)

      case filledArrayInit@FilledArrayInit(Nil, _) =>
        reportError("cannot infer type of empty array, use 'arr <type>[0]' instead", filledArrayInit.getPosition)

      case filledArrayInit@FilledArrayInit(arrayElems, modifiable) =>
        val types = arrayElems.map(check(_, ctx))
        computeJoinOf(types.toSet) match {
          case Some(elemsJoin) =>
            for arrayElem <- arrayElems do {
              markMutPropagation(elemsJoin, arrayElem, ctx)
            }
            ArrayType(elemsJoin, modifiable)
          case None =>
            arrayElems.foreach {
              case VariableRef(name) => ctx.mutIsUsed(name) // avoid false positives
              case _ => ()
            }
            reportError("cannot infer array type", filledArrayInit.getPosition)
        }

      case structInit@StructInit(structName, args, modifiable) =>
        ctx.structs.get(structName) match {
          case Some(structSig: StructSignature) if structSig.isInterface =>
            reportError(
              s"cannot instantiate interface $structName",
              structInit.getPosition
            )
          case Some(structSig: StructSignature) =>
            checkCallArgs(structSig.fields.values.map(_.tpe).toList, args, ctx, structInit.getPosition)
            if (modifiable && !structSig.fields.exists(_._2.isReassignable)){
              reportError(
                s"modification permission granted on struct of type '$structName', but all of its fields are constant",
                structInit.getPosition,
                isWarning = true
              )
            }
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
            reportError(s"operator # can only be applied to arrays and strings, found '$operandType'", unaryOp.getPosition)
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
        val smartCasts = if operator == Operator.And then detectSmartCasts(lhs, ctx) else Map.empty
        val rhsType = check(rhs, ctx.copyWithSmartCasts(smartCasts))
        binOp.setSmartCasts(smartCasts)
        if (operator == Equality || operator == Inequality) {
          val isSubOrSupertype = lhsType.subtypeOf(rhsType) || rhsType.subtypeOf(lhsType)
          if (!isSubOrSupertype) {
            reportError(s"cannot compare '$lhsType' and '$rhsType' using ${Equality.str} or ${Inequality.str}", binOp.getPosition)
          }
          BoolType
        } else {
          mustExistOperator(lhsType, operator, rhsType, binOp.getPosition)
        }

      case select@Select(lhs, selected) =>
        val lhsType = check(lhs, ctx)
        exprMustBeStructWithField(lhsType, selected, ctx, select.getPosition, mustUpdateField = false)

      case varAssig@VarAssig(lhs, rhs) =>
        val rhsType = check(rhs, ctx)
        lhs match {
          case varRef@VariableRef(name) =>
            ctx.varIsAssigned(name)
            val varType = mustBeReassignable(name, ctx, varAssig.getPosition)
            checkSubtypingConstraint(varType, rhsType, varAssig.getPosition, "")
            varRef.setType(varType)
          case indexing@Indexing(indexed, _) =>
            ctx.mutIsUsed(indexing)
            check(indexing, ctx)
            val lhsType = exprMustBeArray(indexed.getType, ctx, varAssig.getPosition, mustUpdate = true)
            checkSubtypingConstraint(lhsType, rhsType, varAssig.getPosition, "")
          case select@Select(structExpr, selected) =>
            ctx.mutIsUsed(select)
            val structType = check(structExpr, ctx)
            val fieldType = exprMustBeStructWithField(structType, selected, ctx, varAssig.getPosition, mustUpdateField = true)
            checkSubtypingConstraint(fieldType, rhsType, varAssig.getPosition, "")
            select.setType(fieldType)
          case _ =>
            reportError("syntax error: only variables, struct fields and array elements can be assigned", varAssig.getPosition)
        }
        markMutPropagation(lhs.getType, rhs, ctx)
        VoidType

      case varModif@VarModif(lhs, rhs, op) =>
        // no check for unused mut because no operator combinable with = can operate on mutable types
        val rhsType = check(rhs, ctx)
        lhs match {
          case varRef@VariableRef(name) =>
            ctx.varIsAssigned(name)
            val inPlaceModifiedType = mustBeReassignable(name, ctx, varModif.getPosition)
            val operatorRetType = mustExistOperator(inPlaceModifiedType, op, rhsType, varModif.getPosition)
            checkSubtypingConstraint(inPlaceModifiedType, operatorRetType, varModif.getPosition, "")
            varRef.setType(inPlaceModifiedType)
          case indexing@Indexing(indexed, _) =>
            ctx.mutIsUsed(indexing)
            check(indexing, ctx)
            val inPlaceModifiedElemType = exprMustBeArray(indexed.getType, ctx, varModif.getPosition, mustUpdate = true)
            val operatorRetType = mustExistOperator(inPlaceModifiedElemType, op, rhsType, varModif.getPosition)
            checkSubtypingConstraint(inPlaceModifiedElemType, operatorRetType, varModif.getPosition, "")
          case select@Select(structExpr, selected) =>
            ctx.mutIsUsed(select)
            val structType = check(structExpr, ctx)
            val inPlaceModifiedFieldType = exprMustBeStructWithField(structType, selected, ctx, varModif.getPosition, mustUpdateField = true)
            val operatorRetType = mustExistOperator(inPlaceModifiedFieldType, op, rhsType, varModif.getPosition)
            checkSubtypingConstraint(inPlaceModifiedFieldType, operatorRetType, varModif.getPosition, "")
            select.setType(inPlaceModifiedFieldType)
          case _ =>
            reportError("syntax error: only variables, struct fields and array elements can be assigned", varModif.getPosition)
        }
        VoidType

      case ifThenElse@IfThenElse(cond, thenBr, elseBrOpt) =>
        val condType = check(cond, ctx)
        checkSubtypingConstraint(BoolType, condType, ifThenElse.getPosition, "if condition")
        val smartCasts = detectSmartCasts(cond, ctx)
        ifThenElse.setSmartCasts(smartCasts)
        check(thenBr, ctx.copyWithSmartCasts(smartCasts))
        elseBrOpt.foreach(check(_, ctx))
        VoidType

      case ternary@Ternary(cond, thenBr, elseBr) =>
        val condType = check(cond, ctx)
        checkSubtypingConstraint(BoolType, condType, ternary.getPosition, "ternary operator condition")
        val smartCasts = detectSmartCasts(cond, ctx)
        ternary.setSmartCasts(smartCasts)
        val thenType = check(thenBr, ctx.copyWithSmartCasts(smartCasts))
        val elseType = check(elseBr, ctx)
        val thenIsSupertype = elseType.subtypeOf(thenType)
        val thenIsSubtype = thenType.subtypeOf(elseType)
        computeJoinOf(Set(thenType, elseType)) match {
          case Some(ternaryType) =>
            markMutPropagation(ternaryType, thenBr, ctx)
            markMutPropagation(ternaryType, elseBr, ctx)
            ternaryType
          case None =>
            reportError(s"cannot infer type of ternary operator: then branch has type '$thenType', " +
              s"else branch has type '$elseType'", ternary.getPosition)
        }

      case whileLoop@WhileLoop(cond, body) =>
        val condType = check(cond, ctx)
        checkSubtypingConstraint(BoolType, condType, whileLoop.getPosition, "while condition")
        val smartCasts = detectSmartCasts(cond, ctx)
        check(body, ctx.copyWithSmartCasts(smartCasts))
        VoidType

      case forLoop@ForLoop(initStats, cond, stepStats, body) =>
        val newCtx = ctx.copied
        initStats.foreach(check(_, newCtx))
        val condType = check(cond, newCtx)
        checkSubtypingConstraint(BoolType, condType, forLoop.getPosition, "for loop condition")
        val smartCasts = detectSmartCasts(cond, ctx)
        val smartCastsAwareCtx = newCtx.copyWithSmartCasts(smartCasts)
        stepStats.foreach(check(_, smartCastsAwareCtx))
        check(body, smartCastsAwareCtx)
        newCtx.writeLocalsRelatedWarnings(errorReporter)
        VoidType

      case ReturnStat(valueOpt) =>
        valueOpt.foreach { value =>
          check(value, ctx)
          value match {
            case VariableRef(name) if ctx.functions.apply(ctx.currentFunctionName.get).retType.maybeModifiable =>
              ctx.mutIsUsed(name)
            case _ => ()
          }
        }
        VoidType

      case cast@Cast(expr, tpe) =>
        val exprType = check(expr, ctx)
        if (exprType.subtypeOf(tpe)) {
          reportError(s"useless conversion: '$exprType' --> '$tpe'", cast.getPosition, isWarning = true)
          tpe
        } else if (TypeConversion.conversionFor(exprType, tpe).isDefined) {
          tpe
        } else {
          structCastResult(exprType, tpe)
            .map { tpe =>
              markMutPropagation(tpe, expr, ctx)
              tpe
            }.getOrElse {
              reportError(s"cannot cast '${expr.getType}' to '$tpe'", cast.getPosition)
            }
        }

      case typeTest@TypeTest(expr, tpe) =>
        val exprType = check(expr, ctx)
        if (exprType.subtypeOf(tpe)){
          reportError(s"test will always be true, as '$exprType' is a subtype of '$tpe'", typeTest.getPosition, isWarning = true)
        } else if (structCastResult(exprType, tpe).isEmpty) {
          reportError(s"cannot check expression of type '$exprType' against type '$tpe'", typeTest.getPosition)
        }
        BoolType

      case panicStat@PanicStat(msg) =>
        val msgType = check(msg, ctx)
        checkSubtypingConstraint(StringType, msgType, panicStat.getPosition, "panic")
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

  private def checkFunCall(name: FunOrVarId, args: List[Expr], pos: Option[Position], ctx: TypeCheckingContext)
                          (using Map[TypeIdentifier, StructSignature]): Type = {
    ctx.functions.get(name) match {
      case Some(funSig) =>
        checkCallArgs(funSig.argTypes, args, ctx, pos)
        funSig.retType
      case None =>
        args.foreach(check(_, ctx))
        reportError(s"not found: $name", pos)
    }
  }

  private def structCastResult(srcType: Type, destType: Type)
                              (using Map[TypeIdentifier, StructSignature]): Option[StructType] = {
    (srcType, destType) match {
      case (StructType(_, srcIsModifiable), StructType(destTypeName, destIsModifiable))
        if destType.subtypeOf(srcType.unmodifiable) && logicalImplies(destIsModifiable, srcIsModifiable)
      => Some(StructType(destTypeName, srcIsModifiable))
      case _ => None
    }
  }

  private def checkCallArgs(expTypes: List[Type], args: List[Expr], ctx: TypeCheckingContext, callPos: Option[Position])
                           (using Map[TypeIdentifier, StructSignature]): Unit = {
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
  
  private def detectSmartCasts(expr: Expr, ctx: TypeCheckingContext): Map[FunOrVarId, Type] = {
    expr match {
      case BinaryOp(lhs, Operator.And, rhs) =>
        // concat order is reversed because we want to keep the leftmost type test in case of conflict
        detectSmartCasts(rhs, ctx) ++ detectSmartCasts(lhs, ctx)
      case TypeTest(VariableRef(varName), tpe) if ctx.getLocalOnly(varName).exists(!_.isReassignable) =>
        Map(varName -> tpe)
      case _ => Map.empty
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
        var alreadyReportedDeadCode = false
        var endStatus = EndStatus(Set.empty, false)
        for stat <- stats do {
          if (endStatus.alwaysStopped && !alreadyReportedDeadCode) {
            reportError("dead code", stat.getPosition)
            alreadyReportedDeadCode = true
          }
          val statEndStatus = checkReturns(stat)
          endStatus = EndStatus(
            endStatus.returned ++ statEndStatus.returned,
            endStatus.alwaysStopped || statEndStatus.alwaysStopped
          )
        }
        endStatus

      case ifThenElse@IfThenElse(_, thenBr, elseBrOpt) =>
        val thenEndStatus = checkReturns(thenBr)
        val elseEndStatus = elseBrOpt.map(checkReturns).getOrElse(EndStatus(Set.empty, alwaysStopped = false))
        EndStatus(thenEndStatus.returned ++ elseEndStatus.returned, thenEndStatus.alwaysStopped && elseEndStatus.alwaysStopped)

      case _: Ternary => EndStatus(Set.empty, false)

      case whileLoop@WhileLoop(_, body) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("while should be replaced by if", whileLoop.getPosition, isWarning = true)
        }
        bodyEndStatus

      case forLoop@ForLoop(_, _, _, body) =>
        val bodyEndStatus = checkReturns(body)
        if (bodyEndStatus.alwaysStopped) {
          reportError("for should be replaced by if", forLoop.getPosition, isWarning = true)
        }
        bodyEndStatus

      case retStat: ReturnStat =>
        val retType = retStat.getRetType
        if (retType.isEmpty) {
          reportError("could not infer type of returned value", retStat.getPosition)
        }
        EndStatus(Set(retType.getOrElse(NothingType)), true)

      case _: PanicStat =>
        EndStatus(Set(NothingType), true)

      case _: (FunDef | StructDef | Param) =>
        assert(false)

      case _ => EndStatus(Set.empty, false)

    }
  }

  private def mustBeKnown(name: FunOrVarId, ctx: TypeCheckingContext, posOpt: Option[Position]): Type = {
    ctx.getLocalOrConst(name) match
      case None =>
        reportError(s"unknown: $name", posOpt)
      case Some(localInfo: LocalInfo) =>
        localInfo.tpe
  }

  private def mustBeReassignable(name: FunOrVarId, ctx: TypeCheckingContext, posOpt: Option[Position]): Type = {
    ctx.getLocalOrConst(name) match
      case None =>
        reportError(s"unknown: $name", posOpt)
      case Some(LocalInfo(_, tpe, isReassignable, _, _, _)) =>
        if (!isReassignable){
          reportError(s"$name is not reassignable", posOpt)
        }
        tpe
  }

  private def exprMustBeArray(exprType: Type, ctx: TypeCheckingContext, posOpt: Option[Position], mustUpdate: Boolean): Type = {
    exprType match
      case ArrayType(elemType, modifiable) =>
        if (mustUpdate && !modifiable){
          reportError("update impossible: missing modification privileges on array", posOpt)
        }
        elemType
      case _ =>
        reportError(s"expected an array, found '$exprType'", posOpt)
  }

  private def mustExistOperator(lhsType: Type, operator: Operator, rhsType: Type, position: Option[Position]): Type = {
    Operators.binaryOperatorSigFor(lhsType, operator, rhsType) match {
      case Some(sig) => sig.retType
      case None =>
        reportError(s"no definition of operator '$operator' found for operands '$lhsType' and '$rhsType'", position)
    }
  }

  private def exprMustBeStructWithField(
                                         exprType: Type,
                                         fieldName: FunOrVarId,
                                         ctx: TypeCheckingContext,
                                         posOpt: Option[Position],
                                         mustUpdateField: Boolean
                                       ): Type = {
    exprType match {
      case StructType(typeName, modifiable) =>
        ctx.structs.apply(typeName) match {
          case StructSignature(_, fields, _, _) if fields.contains(fieldName) =>
            val FieldInfo(fieldType, fieldIsReassig) = fields.apply(fieldName)
            val missingModifPrivilege = mustUpdateField && !modifiable
            if (missingModifPrivilege){
              reportError(s"cannot update field '$fieldName': missing modification privileges on owner struct", posOpt)
            }
            val missingFieldMutability = mustUpdateField && !fieldIsReassig
            if (missingFieldMutability){
              reportError(s"cannot update immutable field '$fieldName'", posOpt)
            }
            fieldType
          case _ =>
            reportError(s"struct '$typeName' has no field named '$fieldName'", posOpt)
        }
      case _ =>
        reportError(s"expected a struct, found '$exprType'", posOpt)
    }
  }

  private def checkSubtypingConstraint(expected: Type, actual: Type, posOpt: Option[Position], msgPrefix: String)
                                      (using Map[TypeIdentifier, StructSignature]): Unit = {
    if (expected != UndefinedType && actual != UndefinedType && !actual.subtypeOf(expected)){
      val fullprefix = if msgPrefix == "" then "" else (msgPrefix ++ ": ")
      reportError(fullprefix ++ s"expected '$expected', found '$actual'", posOpt)
    }
  }

  private def typeMustBeKnown(tpe: Type, ctx: TypeCheckingContext, posOpt: Option[Position]): Type = {
    if (ctx.knowsType(tpe)){
      tpe
    } else {
      reportError(s"unknown type: '$tpe'", posOpt)
    }
  }

  private def computeJoinOf(types: Set[Type])(using structs: Map[TypeIdentifier, StructSignature]): Option[Type] = {
    require(types.nonEmpty)
    if types.size == 1 then Some(types.head)
    else if (types.forall(_.isInstanceOf[StructType])) {
      // if the structs have exactly 1 direct supertype in common, infer it as the result type
      val directSupertypesSets = types.map { tpe =>
        structs.apply(tpe.asInstanceOf[StructType].typeName).directSupertypes.toSet
      }
      val commonDirectSupertypes = directSupertypesSets.reduceLeft(_.intersect(_))
      Some(
        if commonDirectSupertypes.size == 1
        then StructType(commonDirectSupertypes.head, modifiable = types.forall(_.isModifiableForSure))
        else UnionType(types)
      )
    } else {
      // type Nothing should be accepted
      // e.g. in 'when ... then 0 else (panic "")'
      types.find { commonSuper =>
        types.forall(_.subtypeOf(commonSuper))
      }
    }
  }

  private def reportError(msg: String, pos: Option[Position], isWarning: Boolean = false): UndefinedType.type = {
    errorReporter.push(if isWarning then Warning(TypeChecking, msg, pos) else Err(TypeChecking, msg, pos))
    UndefinedType
  }

  private def markMutPropagation(assignedLocalName: FunOrVarId, rhs: Expr, ctx: TypeCheckingContext): Unit = {
    rhs match
      case VariableRef(name) if ctx.getLocalOrConst(assignedLocalName).exists(_.tpe.maybeModifiable) =>
        ctx.mutIsUsed(name)
      case _ => ()
  }

  private def markMutPropagation(expectedType: Type, rhs: Expr, ctx: TypeCheckingContext): Unit = {
    rhs match
      case VariableRef(name) if expectedType.maybeModifiable =>
        ctx.mutIsUsed(name)
      case _ => ()
  }

  private def logicalImplies(p: Boolean, q: Boolean): Boolean = !p || q

  private def shouldNotHappen(): Nothing = {
    throw new AssertionError("should not happen")
  }

}
