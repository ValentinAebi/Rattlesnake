package compiler.typechecker

import compiler.analysisctx.AnalysisContext
import compiler.analysisctx.AnalysisContext.{FunctionFound, FunctionNotFound, ModuleNotFound}
import compiler.gennames.NamesForGeneratedClasses.constantsClassName
import compiler.irs.Asts.*
import compiler.pipeline.CompilationStep.TypeChecking
import compiler.pipeline.CompilerStep
import compiler.reporting.Errors.{Err, ErrorReporter, Warning}
import compiler.reporting.Position
import compiler.typechecker.SubtypeRelation.subtypeOf
import compiler.typechecker.TypeCheckingContext.LocalInfo
import identifiers.{FunOrVarId, IntrinsicsPackageId, NormalTypeId, TypeIdentifier}
import lang.*
import lang.CaptureDescriptors.{Brand, CaptureDescriptor, CaptureSet}
import lang.Operator.{Equality, Inequality, Sharp}
import lang.Operators.{BinaryOpSignature, UnaryOpSignature, binaryOperators, unaryOperators}
import lang.StructSignature.FieldInfo
import lang.Types.*
import lang.Types.PrimitiveTypeShape.*

final class TypeChecker(errorReporter: ErrorReporter)
  extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, analysisContext) = input
    for src <- sources do {
      checkSource(src, analysisContext)
    }
    errorReporter.displayAndTerminateIfErrors()
    sources.foreach(_.assertAllTypesAreSet())
    input
  }
  
  private def checkSource(src: Source, analysisContext: AnalysisContext): Unit = {
    val Source(defs) = src
    for (df <- defs) {
      checkTopLevelDef(df, analysisContext)
    }
  }
  
  private def checkTopLevelDef(topLevelDef: TopLevelDef, analysisContext: AnalysisContext): Unit = topLevelDef match {
    
    case PackageDef(packageName, functions) =>
      val packageSig = analysisContext.resolveType(packageName).get.asInstanceOf[PackageSignature]
      val environment = Environment(
        packageSig.importedPackages.toSet,
        packageSig.importedDevices.toSet
      )
      for func <- functions do {
        checkFunction(func, analysisContext, packageName, packageSig.getCaptureDescr, environment)
      }
      
    case ModuleDef(moduleName, imports, functions) =>
      val moduleSig = analysisContext.resolveType(moduleName).get.asInstanceOf[ModuleSignature]
      val tcCtx = TypeCheckingContext(analysisContext, moduleName, moduleSig.getCaptureDescr)
      for imp <- imports do {
        checkImport(imp, tcCtx)
      }
      val environment = Environment(
        moduleSig.importedPackages.toSet,
        moduleSig.importedDevices.toSet
      )
      for func <- functions do {
        checkFunction(func, analysisContext, moduleName, moduleSig.getCaptureDescr, environment)
      }
      
    case StructDef(structName, fields, _, _) =>
      val structSig = analysisContext.resolveType(structName).get.asInstanceOf[StructSignature]
      val tcCtx = TypeCheckingContext(analysisContext, structName, structSig.getCaptureDescr)
      for (param@Param(paramNameOpt, typeTree, isReassignable) <- fields){
        val tpe = checkType(typeTree)(using tcCtx, analysisContext.unrestrictedEnvironment)
        paramNameOpt.foreach { paramName =>
          tcCtx.addLocal(paramName, tpe, param.getPosition, isReassignable, declHasTypeAnnot = true,
            duplicateVarCallback = { () =>
              reportError(s"duplicate field: $paramName", param.getPosition)
            },
            forbiddenTypeCallback = { () =>
              reportError(s"field $paramName has type $tpe, which is forbidden", param.getPosition)
            }
          )
        }
      }
      
    case constDef@ConstDef(constName, tpeOpt, value) =>
      val inferredType = checkLiteralExpr(value)
      tpeOpt.foreach { expType =>
        val placeholderMeId = NormalTypeId(constantsClassName)
        val tcCtx = TypeCheckingContext(analysisContext, placeholderMeId, CaptureSet.empty)
        val checkedType = checkType(expType)(using tcCtx, Environment.empty)
        checkSubtypingConstraint(checkedType, inferredType, constDef.getPosition, "constant definition", tcCtx)
      }
  }
  
  private def checkFunction(
                             funDef: FunDef,
                             analysisContext: AnalysisContext,
                             meId: TypeIdentifier,
                             meCaptureDescr: CaptureDescriptor,
                             environment: Environment
                           ): Unit = {
    val FunDef(funName, params, optRetTypeTree, body) = funDef
    val tcCtx = TypeCheckingContext(analysisContext, meId, meCaptureDescr)
    for param <- params do {
      val paramType = checkType(param.tpe)(using tcCtx, environment)
      param.paramNameOpt.foreach { paramName =>
        tcCtx.addLocal(paramName, paramType, param.getPosition, param.isReassignable, declHasTypeAnnot = true,
          duplicateVarCallback = { () =>
            reportError(s"identifier '${param.paramNameOpt}' is already used by another parameter of function '$funName'", param.getPosition)
          }, forbiddenTypeCallback = { () =>
            reportError(s"parameter '${param.paramNameOpt}' of function '$funName' has type '$paramType', which is forbidden", param.getPosition)
          })
      }
    }
    val optRetType = optRetTypeTree.map(checkType(_)(using tcCtx, environment))
    val expRetType = optRetType.getOrElse(VoidType)
    checkStat(body)(using tcCtx, environment)
    val endStatus = checkReturns(body)
    if (!endStatus.alwaysStopped && expRetType.subtypeOf(VoidType)(using tcCtx)) {
      reportError("missing return in non-Void function", funDef.getPosition)
    }
    val faultyTypes = endStatus.returned.filter(!_.subtypeOf(expRetType)(using tcCtx))
    if (faultyTypes.nonEmpty) {
      val faultyTypeStr = faultyTypes.map("'" + _ + "'").mkString(", ")
      reportError(s"function '$funName' should return '$expRetType', found $faultyTypeStr", funDef.getPosition)
    }
    if (expRetType == NothingType && !endStatus.alwaysStopped) {
      reportError(s"cannot prove that function '$funName' with return type '$NothingType' cannot return", funDef.getPosition)
    }
    tcCtx.writeLocalsRelatedWarnings(errorReporter)
  }
  
  private def checkImport(imp: Import, tcCtx: TypeCheckingContext): Unit = imp match {
    case modImp@ParamImport(paramName, paramType) =>
      val tpe = checkType(paramType)(using tcCtx, tcCtx.unrestrictedEnvironment)
      tcCtx.addLocal(paramName, tpe, modImp.getPosition, isReassignable = false, declHasTypeAnnot = true,
        duplicateVarCallback = { () =>
          reportError(s"duplicated parameter: $paramName", modImp.getPosition)
        },
        forbiddenTypeCallback = { () =>
          reportError(s"module $paramName has type $paramType, which is forbidden", modImp.getPosition)
        }
      )
    case pkgImp@PackageImport(packageId) =>
      if (!tcCtx.knowsUserDefType(packageId)){
        reportError(s"unknown package: $packageId", imp.getPosition)
      }
    case DeviceImport(device) => ()
  }
  
  private def checkType(typeTree: TypeTree)(using tcCtx: TypeCheckingContext, env: Environment): Type = {
    val capDescrOpt = typeTree.captureDescrOpt.map(checkCaptureDescr)
    val shape = typeTree match {
      case PrimitiveTypeShapeTree(primitiveType, _) =>
        primitiveType
      case NamedTypeShapeTree(name, _) =>
        if (!tcCtx.knowsUserDefType(name)){
          reportError(s"unknown type: $name", typeTree.getPosition)
        } else NamedTypeShape(name)
      case ArrayTypeShapeTree(elemTypeTree, _, isModifiable) =>
        val elemType = checkType(elemTypeTree)
        ArrayTypeShape(elemType, isModifiable)
    }
    shape ^ capDescrOpt
  }

  private def checkCaptureDescr(captureDescrTree: CaptureDescrTree)
                               (using tcCtx: TypeCheckingContext, env: Environment): CaptureDescriptor = {
    captureDescrTree match {
      case ExplicitCaptureSetTree(capturedExpressions) =>
        CaptureSet(capturedExpressions.flatMap { expr =>
          checkExpr(expr)
          PathsConverter.convertAndCheck(expr, tcCtx, errorReporter)
        }.toSet)
      case ImplicitRootCaptureSetTree() => CaptureSet.singletonOfRoot
      case BrandTree() => Brand
    }
  }

  private def checkStat(statement: Statement)(using tcCtx: TypeCheckingContext, env: Environment): Unit = statement match {
    
    case expr: Expr => checkExpr(expr)
    
    case Block(stats) =>
      val newCtx = tcCtx.copied
      for stat <- stats do {
        checkStat(stat)(using newCtx)
      }
      newCtx.writeLocalsRelatedWarnings(errorReporter)

    case localDef@LocalDef(localName, optTypeAnnotTree, rhs, isReassignable) =>
      val inferredType = checkExpr(rhs)
      val optAnnotType = optTypeAnnotTree.map { typeAnnot =>
        val tpe = checkType(typeAnnot)(using tcCtx, tcCtx.unrestrictedEnvironment)
        checkSubtypingConstraint(tpe, inferredType, localDef.getPosition, "local definition", tcCtx)
        tpe
      }
      val requestExplicitType = optTypeAnnotTree.isEmpty && inferredType.isInstanceOf[UnionTypeShape]
      if (requestExplicitType) {
        reportError(s"Please provide an explicit type for $localName", localDef.getPosition)
      }
      val actualType = if requestExplicitType then UndefinedTypeShape else optAnnotType.getOrElse(inferredType)
      localDef.setVarType(actualType)
      tcCtx.addLocal(localName, actualType, localDef.getPosition, isReassignable,
        declHasTypeAnnot = optTypeAnnotTree.isDefined,
        duplicateVarCallback = { () =>
          reportError(s"'$localName' is already defined in this scope", localDef.getPosition)
        }, forbiddenTypeCallback = { () =>
          reportError(s"${localDef.keyword} '$localName' has type '$actualType', which is forbidden", localDef.getPosition)
        })
      
    case varAssig@VarAssig(lhs, rhs) =>
      val rhsType = checkExpr(rhs)
      lhs match {
        case varRef@VariableRef(name) =>
          tcCtx.varIsAssigned(name)
          val varType = mustBeReassignable(name, tcCtx, varAssig.getPosition)
          checkSubtypingConstraint(varType, rhsType, varAssig.getPosition, "", tcCtx)
          varRef.setType(varType)
        case indexing@Indexing(indexed, _) =>
          checkExpr(indexing)
          val lhsType = exprMustBeArray(indexed.getType, tcCtx, varAssig.getPosition, mustUpdate = true)
          checkSubtypingConstraint(lhsType, rhsType, varAssig.getPosition, "", tcCtx)
        case select@Select(structExpr, selected) =>
          val structType = checkExpr(structExpr)
          val fieldType = exprMustHaveField(structExpr, selected, tcCtx, varAssig.getPosition, mustUpdateField = true)
          checkSubtypingConstraint(fieldType, rhsType, varAssig.getPosition, "", tcCtx)
          select.setType(fieldType)
        case _ =>
          reportError("syntax error: only variables, struct fields and array elements can be assigned", varAssig.getPosition)
      }

    case varModif@VarModif(lhs, rhs, op) =>
      // no check for unused mut because no operator combinable with = can operate on mutable types
      val rhsType = checkExpr(rhs)
      lhs match {
        case varRef@VariableRef(name) =>
          tcCtx.varIsAssigned(name)
          val inPlaceModifiedType = mustBeReassignable(name, tcCtx, varModif.getPosition)
          val operatorRetType = mustExistOperator(inPlaceModifiedType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedType, operatorRetType, varModif.getPosition, "", tcCtx)
          varRef.setType(inPlaceModifiedType)
        case indexing@Indexing(indexed, _) =>
          checkExpr(indexing)
          val inPlaceModifiedElemType = exprMustBeArray(indexed.getType, tcCtx, varModif.getPosition, mustUpdate = true)
          val operatorRetType = mustExistOperator(inPlaceModifiedElemType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedElemType, operatorRetType, varModif.getPosition, "", tcCtx)
        case select@Select(structExpr, selected) =>
          val structType = checkExpr(structExpr)
          val inPlaceModifiedFieldType = exprMustHaveField(structExpr, selected, tcCtx, varModif.getPosition, mustUpdateField = true)
          val operatorRetType = mustExistOperator(inPlaceModifiedFieldType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedFieldType, operatorRetType, varModif.getPosition, "", tcCtx)
          select.setType(inPlaceModifiedFieldType)
        case _ =>
          reportError("syntax error: only variables, struct fields and array elements can be assigned", varModif.getPosition)
      }

    case ifThenElse@IfThenElse(cond, thenBr, elseBrOpt) =>
      val condType = checkExpr(cond)
      checkSubtypingConstraint(BoolType, condType, ifThenElse.getPosition, "if condition", tcCtx)
      val smartCasts = detectSmartCasts(cond, tcCtx)
      ifThenElse.setSmartCasts(smartCasts)
      checkStat(thenBr)(using tcCtx.copyWithSmartCasts(smartCasts))
      elseBrOpt.foreach(checkStat)

    case whileLoop@WhileLoop(cond, body) =>
      val condType = checkExpr(cond)
      checkSubtypingConstraint(BoolType, condType, whileLoop.getPosition, "while condition", tcCtx)
      val smartCasts = detectSmartCasts(cond, tcCtx)
      checkStat(body)(using tcCtx.copyWithSmartCasts(smartCasts))

    case forLoop@ForLoop(initStats, cond, stepStats, body) =>
      val newCtx = tcCtx.copied
      initStats.foreach(checkStat(_)(using newCtx))
      val condType = checkExpr(cond)(using newCtx)
      checkSubtypingConstraint(BoolType, condType, forLoop.getPosition, "for loop condition", tcCtx)
      val smartCasts = detectSmartCasts(cond, tcCtx)
      val smartCastsAwareCtx = newCtx.copyWithSmartCasts(smartCasts)
      stepStats.foreach(checkStat(_)(using smartCastsAwareCtx))
      checkStat(body)(using smartCastsAwareCtx)
      newCtx.writeLocalsRelatedWarnings(errorReporter)

    case ReturnStat(valueOpt) =>
      valueOpt.foreach { value =>
        checkExpr(value)
      }

    case panicStat@PanicStat(msg) =>
      val msgType = checkExpr(msg)
      checkSubtypingConstraint(StringType, msgType, panicStat.getPosition, "panic", tcCtx)

    case EnclosedStat(capabilities, body) =>
      for (capability <- capabilities) {
        val capabilityType = checkExpr(capability)
        // TODO support devices, and maybe structs and arrays
        checkSubtypingConstraint(RegionType, capabilityType, capability.getPosition, "enclosure permission", tcCtx)
      }
      checkStat(body)
  }
  
  private def checkLiteralExpr(literal: Literal): Type = literal match {
    case _: IntLit => IntType
    case _: DoubleLit => DoubleType
    case _: CharLit => CharType
    case _: BoolLit => BoolType
    case _: StringLit => StringType
  }

  private def checkExpr(expr: Expr)(using tcCtx: TypeCheckingContext, env: Environment): Type = {
    val tpe = expr match {

      case literal: Literal =>
        checkLiteralExpr(literal)
      
      case varRef@VariableRef(name) =>
        tcCtx.localIsQueried(name)
        tcCtx.getLocalOrConst(name) match
          case None =>
            reportError(s"unknown: $name", varRef.getPosition)
          case Some(localInfo: LocalInfo) =>
            localInfo.tpe

      case MeRef() =>
        tcCtx.meType

      case pkg@PackageRef(packageName) =>
        if (!env.allowsPackage(packageName)) {
          reportError(s"illegal reference to $packageName, which is not imported in this module", pkg.getPosition)
        }
        tcCtx.resolveType(packageName) match {
          case Some(packageSignature: PackageSignature) => NamedTypeShape(packageName)
          case Some(_) => reportError(s"$packageName is not a package and is thus not allowed here", pkg.getPosition)
          case None => reportError(s"not found: $packageName", pkg.getPosition)
        }

      case devRef@DeviceRef(device) =>
        if (!env.allowsDevice(device)) {
          reportError(s"illegal reference to device $device, which is not imported in this module", devRef.getPosition)
        }
        NamedTypeShape(device.sig.id)

      case call@Call(None, funName, args) =>
        checkFunCall(call, IntrinsicsPackageId, fallbackOwnerOpt = Some(tcCtx.meId))

      case call@Call(Some(receiver), funName, args) =>
        checkExpr(receiver) match {
          case namedType: NamedTypeShape =>
            checkFunCall(call, namedType.typeName, None)
          case recType =>
            reportError(s"expected a module or package type, found $recType", receiver.getPosition)
        }

      case indexing@Indexing(indexed, arg) =>
        val indexedType = checkExpr(indexed)
        val argType = checkExpr(arg)
        checkSubtypingConstraint(IntType, argType, indexing.getPosition, "array index", tcCtx)
        exprMustBeArray(indexed.getType, tcCtx, indexed.getPosition, mustUpdate = false)

      case arrayInit@ArrayInit(region, elemTypeTree, size) =>
        checkAndRequireRegion(region, tcCtx)
        if (elemTypeTree == VoidType || elemTypeTree == NothingType) {
          reportError(s"array cannot have element type $elemTypeTree", arrayInit.getPosition)
        }
        val sizeType = checkExpr(size)
        checkSubtypingConstraint(IntType, sizeType, size.getPosition, "array size", tcCtx)
        size match {
          case IntLit(value) if value < 0 =>
            reportError("array size should be nonnegative", size.getPosition)
          case _ => ()
        }
        val elemType = checkType(elemTypeTree)
        ArrayTypeShape(elemType, modifiable = true)

      case filledArrayInit@FilledArrayInit(regionOpt, Nil) =>
        regionOpt.foreach(checkAndRequireRegion(_, tcCtx))
        reportError("cannot infer type of empty array, use 'arr <type>[0]' instead", filledArrayInit.getPosition)

      case filledArrayInit@FilledArrayInit(regionOpt, arrayElems) =>
        regionOpt.foreach(checkAndRequireRegion(_, tcCtx))
        val types = arrayElems.map(checkExpr)
        computeJoinOf(types.toSet, tcCtx) match {
          case Some(elemsJoin) =>
            ArrayTypeShape(elemsJoin, modifiable = regionOpt.isDefined)
          case None =>
            reportError("cannot infer array type", filledArrayInit.getPosition)
        }

      case instantiation@StructOrModuleInstantiation(regionOpt, tid, args) =>
        regionOpt.foreach(checkAndRequireRegion(_, tcCtx))
        tcCtx.resolveType(tid) match {
          case Some(structSig: StructSignature) if structSig.isInterface =>
            reportError(
              s"cannot instantiate interface $tid",
              instantiation.getPosition
            )
          case Some(structSig: StructSignature) =>
            if (structSig.isShallowMutable && regionOpt.isEmpty){
              reportError(
                s"cannot instantiate '$tid' without providing a region, since at least one of its fields is reassignable",
                instantiation.getPosition
              )
            }
            checkCallArgs(structSig.fields.values.map(_.tpe).toList, args, tcCtx, instantiation.getPosition)
            NamedTypeShape(tid)
          case Some(moduleSig: ModuleSignature) =>
            val expectedTypes = moduleSig.paramImports.values.toList
            checkCallArgs(expectedTypes, args, tcCtx, instantiation.getPosition)
            NamedTypeShape(tid)
          case _ => reportError(s"not found: structure or module '$tid'", instantiation.getPosition)
        }

      case RegionCreation() =>
        RegionType

      case unaryOp@UnaryOp(operator, operand) =>
        val operandType = checkExpr(operand)
        if (operator == Sharp) {
          if (operandType.isInstanceOf[ArrayTypeShape] || operandType == StringType) {
            IntType
          } else {
            reportError(s"operator # can only be applied to arrays and strings, found '$operandType'", unaryOp.getPosition)
          }
        } else {
          unaryOperatorSignatureFor(operator, operandType) match {
            case Some(sig) => sig.retType
            case None =>
              reportError(s"no definition of operator '$operator' found for operand '$operandType'", unaryOp.getPosition)
          }
        }

      case binOp@BinaryOp(lhs, operator, rhs) =>
        // no check for unused mut because no binary operator requires mutability on its arguments
        val lhsType = checkExpr(lhs)
        val smartCasts = if operator == Operator.And then detectSmartCasts(lhs, tcCtx) else Map.empty
        val rhsType = checkExpr(rhs)(using tcCtx.copyWithSmartCasts(smartCasts), env)
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
        val lhsType = checkExpr(lhs)
        exprMustHaveField(lhs, selected, tcCtx, select.getPosition, mustUpdateField = false)

      case ternary@Ternary(cond, thenBr, elseBr) =>
        val condType = checkExpr(cond)
        checkSubtypingConstraint(BoolType, condType, ternary.getPosition, "ternary operator condition", tcCtx)
        val smartCasts = detectSmartCasts(cond, tcCtx)
        ternary.setSmartCasts(smartCasts)
        val thenType = checkExpr(thenBr)(using tcCtx.copyWithSmartCasts(smartCasts), env)
        val elseType = checkExpr(elseBr)
        val thenIsSupertype = elseType.subtypeOf(thenType)
        val thenIsSubtype = thenType.subtypeOf(elseType)
        computeJoinOf(Set(thenType, elseType), tcCtx) match {
          case Some(ternaryType) =>
            ternaryType
          case None =>
            reportError(s"cannot infer type of ternary operator: then branch has type '$thenType', " +
              s"else branch has type '$elseType'", ternary.getPosition)
        }

      case cast@Cast(expr, typeTree) =>
        val exprType = checkExpr(expr)
        val tpe = checkType(typeTree)
        if (exprType.subtypeOf(tpe)) {
          reportError(s"useless conversion: '$exprType' --> '$tpe'", cast.getPosition, isWarning = true)
          cast.markTransparent()
          tpe
        } else if (TypeConversion.conversionFor(exprType, tpe).isDefined) {
          tpe
        } else {
          structCastResult(exprType, tpe, tcCtx).getOrElse {
            reportError(s"cannot cast '${expr.getType}' to '$tpe'", cast.getPosition)
          }
        }

      case typeTest@TypeTest(expr, typeTree) =>
        val exprType = checkExpr(expr)
        val tpe = checkType(typeTree)
        if (exprType.subtypeOf(tpe)) {
          reportError(s"test will always be true, as '$exprType' is a subtype of '$tpe'", typeTest.getPosition, isWarning = true)
        } else if (structCastResult(exprType, tpe, tcCtx).isEmpty) {
          reportError(s"cannot check expression of type '$exprType' against type '$tpe'", typeTest.getPosition)
        }
        BoolType
      
      case _: Sequence => throw AssertionError("should not happen, as Sequences are produced by the desugaring phase")
    }
    expr.setType(tpe)
    tpe
  }

  private def checkAndRequireRegion(region: Expr, ctx: TypeCheckingContext)(using TypeCheckingContext, Environment): Unit = {
    val regType = checkExpr(region)
    checkSubtypingConstraint(PrimitiveTypeShape.RegionType, regType, region.getPosition, "region", ctx)
  }

  private def unaryOperatorSignatureFor(operator: Operator, operand: Type)
                                       (using TypeCheckingContext, Environment): Option[UnaryOpSignature] = {
    unaryOperators.find {
      case UnaryOpSignature(op, operandType, _) =>
        operator == op && operand.subtypeOf(operandType)
    }
  }

  private def binaryOperatorSigFor(left: Type, operator: Operator, right: Type)
                                  (using TypeCheckingContext, Environment): Option[BinaryOpSignature] = {
    binaryOperators.find {
      case BinaryOpSignature(leftOperandType, op, rightOperandType, _) =>
        left.subtypeOf(leftOperandType) && op == operator && right.subtypeOf(rightOperandType)
    }
  }

  /**
   * @return (meType, callType)
   */
  private def checkFunCall(
                            call: Call,
                            owner: TypeIdentifier,
                            fallbackOwnerOpt: Option[TypeIdentifier]
                          )(using tcCtx: TypeCheckingContext, envir: Environment): Type = {
    val funName = call.function
    val args = call.args
    val pos = call.getPosition
    tcCtx.resolveFunc(owner, funName) match {
      case FunctionFound(funSig) =>
        checkCallArgs(funSig.argTypes, args, tcCtx, pos)
        call.resolve(funSig)
        funSig.retType
      case ModuleNotFound =>
        args.foreach(checkExpr)
        reportError(s"not found: package or module $owner", pos)
      case FunctionNotFound =>
        fallbackOwnerOpt map { fallbackOwner =>
          checkFunCall(call, fallbackOwner, None)
        } getOrElse {
          args.foreach(checkExpr)
          reportError(s"function not found: '$funName'", pos)
        }
    }
  }

  private def structCastResult(srcType: Type, destType: Type, ctx: TypeCheckingContext)
                              (using TypeCheckingContext, Environment): Option[NamedTypeShape] = {
    (srcType, destType) match {
      case (srcType: NamedTypeShape, destType: NamedTypeShape)
        if destType.subtypeOf(srcType) || srcType.subtypeOf(destType)
      => Some(destType)
      case _ => None
    }
  }

  private def checkCallArgs(expTypes: List[TypeShape], args: List[Expr], ctx: TypeCheckingContext, callPos: Option[Position])
                           (using TypeCheckingContext, Environment): Unit = {
    val expTypesIter = expTypes.iterator
    val argsIter = args.iterator
    var errorFound = false
    while (expTypesIter.hasNext && argsIter.hasNext && !errorFound) {
      val expType = expTypesIter.next()
      val arg = argsIter.next()
      val actType = checkExpr(arg)
      if (!actType.subtypeOf(expType)) {
        reportError(s"expected '$expType', found '$actType'", arg.getPosition)
        errorFound = true
      }
    }
    if (expTypesIter.hasNext && !errorFound) {
      reportError(s"expected argument of type '${expTypesIter.next()}', found end of arguments list", callPos)
    }
    if (argsIter.hasNext && !errorFound) {
      val arg = argsIter.next()
      reportError(s"expected end of arguments list, found argument of type '${checkExpr(arg)}'", arg.getPosition)
    }
    for arg <- argsIter do {
      checkExpr(arg)
    }
  }

  private def detectSmartCasts(expr: Expr, ctx: TypeCheckingContext): Map[FunOrVarId, Type] = {
    expr match {
      case BinaryOp(lhs, Operator.And, rhs) =>
        // concat order is reversed because we want to keep the leftmost type test in case of conflict
        detectSmartCasts(rhs, ctx) ++ detectSmartCasts(lhs, ctx)
      case TypeTest(VariableRef(varName), typeTree) if ctx.getLocalOnly(varName).exists(!_.isReassignable) =>
        typeTree.getResolvedType.map { tpe =>
          val castedType = 
          Map(varName -> tpe)
        }.getOrElse(Map.empty)
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

      case EnclosedStat(_, body) =>
        checkReturns(body)

      case _: (FunDef | StructDef | Param) =>
        assert(false)

      case _ => EndStatus(Set.empty, false)

    }
  }

  private def mustBeReassignable(name: FunOrVarId, ctx: TypeCheckingContext, posOpt: Option[Position]): Type = {
    ctx.getLocalOrConst(name) match
      case None =>
        reportError(s"unknown: $name", posOpt)
      case Some(LocalInfo(_, tpe, isReassignable, _, _, _)) =>
        if (!isReassignable) {
          reportError(s"$name is not reassignable", posOpt)
        }
        tpe
  }

  private def exprMustBeArray(exprType: Type, ctx: TypeCheckingContext, posOpt: Option[Position], mustUpdate: Boolean): Type = {
    exprType match
      case ArrayTypeShape(elemType, modifiable) =>
        if (mustUpdate && !modifiable) {
          reportError("update impossible: missing modification privileges on array", posOpt)
        }
        elemType
      case _ =>
        reportError(s"expected an array, found '$exprType'", posOpt)
  }

  private def mustExistOperator(lhsType: Type, operator: Operator, rhsType: Type, position: Option[Position])
                               (using TypeCheckingContext, Environment): Type = {
    binaryOperatorSigFor(lhsType, operator, rhsType) match {
      case Some(sig) => sig.retType
      case None =>
        reportError(s"no definition of operator '$operator' found for operands '$lhsType' and '$rhsType'", position)
    }
  }

  private def exprMustHaveField(
                                 expr: Expr,
                                 fieldName: FunOrVarId,
                                 ctx: TypeCheckingContext,
                                 posOpt: Option[Position],
                                 mustUpdateField: Boolean
                               ): Type = {
    val exprType = expr.getType
    exprType match {
      case NamedTypeShape(typeName) =>
        ctx.resolveType(typeName) match {
          case Some(StructSignature(_, fields, _, _)) if fields.contains(fieldName) =>
            val FieldInfo(fieldType, fieldIsReassig) = fields.apply(fieldName)
            val missingFieldMutability = mustUpdateField && !fieldIsReassig
            if (missingFieldMutability) {
              reportError(s"cannot update immutable field '$fieldName'", posOpt)
            }
            fieldType
          case Some(ModuleSignature(_, paramImports, _, _, _)) if paramImports.contains(fieldName) =>
            if (!expr.isInstanceOf[MeRef]) {
              reportError(s"references to module imports are only allowed when using the '${Keyword.Me}' keyword", posOpt)
            }
            if (mustUpdateField) {
              reportError("cannot update module field", posOpt)
            }
            paramImports.apply(fieldName)
          case _ =>
            reportError(s"'$exprType' has no field named '$fieldName'", posOpt)
        }
      case _ =>
        reportError(s"'$exprType' has no field named '$fieldName'", posOpt)
    }
  }

  private def checkSubtypingConstraint(
                                        expected: Type,
                                        actual: Type,
                                        posOpt: Option[Position],
                                        msgPrefix: String,
                                        ctx: TypeCheckingContext
                                      ): Unit = {
    if (expected != UndefinedTypeShape && actual != UndefinedTypeShape && !actual.subtypeOf(expected)) {
      val fullprefix = if msgPrefix == "" then "" else (msgPrefix ++ ": ")
      reportError(fullprefix ++ s"expected '$expected', found '$actual'", posOpt)
    }
  }

  private def computeJoinOf(types: Set[Type], ctx: TypeCheckingContext): Option[Type] = {
    require(types.nonEmpty)
    if types.size == 1 then Some(types.head)
    else if (areAllStructs(types, ctx)) {
      val structs = ctx.structs
      // if the structs have exactly 1 direct supertype in common, infer it as the result type
      val directSupertypesSets = types.map { tpe =>
        structs.apply(tpe.shape.asInstanceOf[NamedTypeShape].typeName).directSupertypes.toSet
      }
      val commonDirectSupertypes = directSupertypesSets.reduceLeft(_.intersect(_))
      Some(
        if commonDirectSupertypes.size == 1
        then {
          val superT = commonDirectSupertypes.head
          NamedTypeShape(superT)
        } else {
          val cd = 
          UnionTypeShape(types)
        }
      )
    } else {
      // type Nothing should be accepted
      // e.g. in 'when ... then 0 else (panic "")'
      types.find { commonSuper =>
        types.forall(_.subtypeOf(commonSuper)(using ctx))
      }
    }
  }

  private def areAllStructs(types: Set[Type], ctx: TypeCheckingContext): Boolean = {
    types.forall {
      case NamedTypeShape(typeName) => ctx.resolveType(typeName).exists(_.isInstanceOf[StructSignature])
      case _ => false
    }
  }

  private def reportError(msg: String, pos: Option[Position], isWarning: Boolean = false): UndefinedTypeShape.type = {
    errorReporter.push(if isWarning then Warning(TypeChecking, msg, pos) else Err(TypeChecking, msg, pos))
    UndefinedTypeShape
  }

  private def logicalImplies(p: Boolean, q: Boolean): Boolean = !p || q

  private def shouldNotHappen(): Nothing = {
    throw new AssertionError("should not happen")
  }

}
