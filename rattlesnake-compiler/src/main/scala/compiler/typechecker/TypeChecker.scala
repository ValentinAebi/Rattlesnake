package compiler.typechecker

import compiler.analysisctx.AnalysisContext
import compiler.analysisctx.AnalysisContext.{FunctionFound, FunctionNotFound, ModuleNotFound}
import compiler.gennames.ClassesAndDirectoriesNames.constantsClassName
import compiler.irs.Asts.*
import compiler.pipeline.CompilationStep.TypeChecking
import compiler.pipeline.CompilerStep
import compiler.reporting.Errors.{Err, ErrorReporter, Warning}
import compiler.reporting.Position
import compiler.typechecker.SubcaptureRelation.{isCoveredBy, subcaptureOf}
import compiler.typechecker.SubtypeRelation.subtypeOf
import compiler.typechecker.TypeCheckingContext.LocalInfo
import identifiers.*
import lang.*
import lang.Capturables.*
import lang.CaptureDescriptors.{CaptureDescriptor, CaptureSet, Mark}
import lang.LanguageMode.*
import lang.Operator.{Equality, Inequality, Sharp}
import lang.Operators.{BinaryOpSignature, UnaryOpSignature, binaryOperators, unaryOperators}
import lang.Types.*
import lang.Types.PrimitiveTypeShape.*

import scala.collection.mutable.ListBuffer

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
    val Source(defs, languageMode) = src
    for (df <- defs) {
      checkTopLevelDef(df, analysisContext)(using languageMode)
    }
  }

  private def checkTopLevelDef(topLevelDef: TopLevelDef, analysisContext: AnalysisContext)
                              (using langMode: LanguageMode): Unit = topLevelDef match {

    case PackageDef(packageName, functions) =>
      val packageSig = analysisContext.resolveTypeAs[PackageSignature](packageName).get
      val environment = packageSig.getNonSubstitutedCaptureDescr
      val mainFunctionsCollector = ListBuffer.empty[FunDef]
      for func <- functions do {
        checkFunction(func, analysisContext, packageName, packageSig.getNonSubstitutedCaptureDescr, langMode,
          packageSig.importedPackages.toSet, packageSig.importedDevices.toSet, Some(mainFunctionsCollector))
      }
      if (mainFunctionsCollector.size > 1) {
        for (mainFun <- mainFunctionsCollector) {
          reportError("more than one main function in the current package", mainFun.getPosition)
        }
      }

    case ModuleDef(moduleName, imports, functions) =>
      val moduleSig = analysisContext.resolveTypeAs[ModuleSignature](moduleName).get
      val importsCtx = TypeCheckingContext(
        analysisContext,
        RootEnvir,
        meTypeId = moduleName,
        meCaptureDescr = moduleSig.getNonSubstitutedCaptureDescr,
        moduleSig.importedPackages.toSet,
        moduleSig.importedDevices.toSet
      )
      for imp <- imports do {
        checkImport(imp, importsCtx, OcapEnabled)
      }
      val environment = moduleSig.getNonSubstitutedCaptureDescr
      for func <- functions do {
        checkFunction(func, analysisContext, moduleName, moduleSig.getNonSubstitutedCaptureDescr, OcapEnabled,
          moduleSig.importedPackages.toSet, moduleSig.importedDevices.toSet, None)
      }

    case StructDef(structName, fields, _, _) =>
      val structSig = analysisContext.resolveTypeAs[StructSignature](structName).get
      val tcCtx = TypeCheckingContext(
        analysisContext,
        RootEnvir,
        meTypeId = structName,
        meCaptureDescr = structSig.getNonSubstitutedCaptureDescr,
        analysisContext.packages.keySet,
        Device.values.toSet
      )
      for (param@Param(paramNameOpt, typeTree, isReassignable) <- fields) {
        val tpe = checkType(typeTree, idsAreFields = true)(using tcCtx, langMode)
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
        if (param.isReassignable) {
          typeShouldNotCaptureRoot(tpe, s"reassignable field '${param.paramNameOpt.getOrElse("<missing name>")}' " +
            "cannot capture the root capability", tcCtx, param.getPosition)
        }
      }

    case constDef@ConstDef(constName, tpeOpt, value) =>
      val inferredType = checkLiteralExpr(value)
      tpeOpt.foreach { expType =>
        val placeholderMeId = NormalTypeId(constantsClassName)
        val tcCtx = TypeCheckingContext(
          analysisContext,
          RootEnvir,
          meTypeId = placeholderMeId,
          meCaptureDescr = CaptureSet.empty,
          allowedPackages = Set.empty,
          allowedDevices = Set.empty
        )
        val checkedType = checkType(expType, idsAreFields = false)(using tcCtx, OcapDisabled)
        checkSubtypingConstraint(
          checkedType,
          inferredType,
          constDef.getPosition,
          "constant definition"
        )(using tcCtx, langMode)
      }
  }

  private def typeShouldNotCaptureRoot(tpe: Type, msg: String, tcCtx: TypeCheckingContext, posOpt: Option[Position]) = {
    tpe.captureDescriptor match {
      case cs: CaptureSet if RootCapability.isCoveredBy(cs)(using tcCtx) =>
        reportError(msg, posOpt)
      case _ => ()
    }
  }

  private def checkFunction(
                             funDef: FunDef,
                             analysisContext: AnalysisContext,
                             meId: TypeIdentifier,
                             meCaptureDescr: CaptureDescriptor,
                             langMode: LanguageMode,
                             allowedPackages: Set[TypeIdentifier],
                             allowedDevices: Set[Device],
                             mainFunctionsCollectorOpt: Option[ListBuffer[FunDef]]
                           ): Unit = {
    val FunDef(funName, params, optRetTypeTree, body, isMain) = funDef
    if (isMain) {
      checkIsEligibleAsMain(funDef)
      mainFunctionsCollectorOpt.foreach(_.addOne(funDef))
    }
    if (isMain && mainFunctionsCollectorOpt.isEmpty) {
      reportError("only packages can contain a main function", funDef.getPosition)
    }
    val tcCtx = TypeCheckingContext(analysisContext, RootEnvir, meId, meCaptureDescr, allowedPackages, allowedDevices)
    for param <- params do {
      val paramType = checkType(param.tpe, idsAreFields = false)(using tcCtx, langMode)
      param.paramNameOpt.foreach { paramName =>
        tcCtx.addLocal(paramName, paramType, param.getPosition, param.isReassignable, declHasTypeAnnot = true,
          duplicateVarCallback = { () =>
            reportError(s"identifier '${param.paramNameOpt}' is already used by another parameter of function '$funName'", param.getPosition)
          }, forbiddenTypeCallback = { () =>
            reportError(s"parameter '${param.paramNameOpt}' of function '$funName' has type '$paramType', which is forbidden", param.getPosition)
          })
      }
      if (param.isReassignable && param.paramNameOpt.isEmpty) {
        reportError("unnamed reassignable parameter", param.getPosition, isWarning = true)
      }
    }
    val optRetType = optRetTypeTree.map(checkType(_, idsAreFields = false)(using tcCtx, langMode))
    val expRetType = optRetType.getOrElse(VoidType)
    checkStat(body)(using tcCtx, langMode, expRetType)
    tcCtx.writeLocalsRelatedWarnings(errorReporter)
  }

  private def checkIsEligibleAsMain(funDef: FunDef): Unit = {
    import ArrayTypeShapeTree as ArrSh
    import PrimitiveTypeShapeTree as PrimSh
    funDef match {
      case FunDef(_, List(Param(_, ArrSh(PrimSh(StringType), _), _)), _, _, _) => ()
      case _ =>
        val funSig = funDef.getSignatureOpt.get
        val expectedHeader =
          s"${Keyword.Main} ${Keyword.Fn} ${funDef.funName}(${ArrayTypeShape(StringType, false)}) -> ${funSig.retType}"
        reportError(s"main function should have the following header: $expectedHeader", funDef.getPosition)
    }
  }

  private def checkImport(imp: Import, tcCtx: TypeCheckingContext, langMode: LanguageMode): Unit = imp match {
    case modImp@ParamImport(paramName, paramType) =>
      val tpe = checkType(paramType, idsAreFields = true)(using tcCtx, langMode)
      tcCtx.addLocal(paramName, tpe, modImp.getPosition, isReassignable = false, declHasTypeAnnot = true,
        duplicateVarCallback = { () =>
          reportError(s"duplicated parameter: $paramName", modImp.getPosition)
        },
        forbiddenTypeCallback = { () =>
          reportError(s"module $paramName has type $paramType, which is forbidden", modImp.getPosition)
        }
      )
    case pkgImp@PackageImport(packageId) =>
      if (!tcCtx.knowsUserDefType(packageId)) {
        reportError(s"unknown package: $packageId", imp.getPosition)
      }
    case DeviceImport(device) => ()
  }

  private def checkType(typeTree: TypeTree, idsAreFields: Boolean)
                       (using tcCtx: TypeCheckingContext, langMode: LanguageMode): Type = {
    val tpe = typeTree match {
      case capTypeTree@CapturingTypeTree(typeShapeTree, captureDescrTree) =>
        featureIsNotAllowedIfOcapDisabled("capturing types", capTypeTree.getPosition)
        val shape = checkTypeShape(typeShapeTree, idsAreFields)
        val descriptor = checkCaptureDescr(captureDescrTree, idsAreFields)
        if (langMode == OcapEnabled) {
          warnOnNonCapAndRedundantCaptures(captureDescrTree)
        }
        CapturingType(shape, descriptor)
      case typeShapeTree: TypeShapeTree =>
        checkTypeShape(typeShapeTree, idsAreFields)
      case WrapperTypeTree(tpe) => tpe
    }
    if (langMode == OcapEnabled) {
      warnWhenPrimitiveHasInappropriateCaptureDescr(tpe, typeTree.getPosition)
    }
    tpe
  }

  private def warnWhenPrimitiveHasInappropriateCaptureDescr(tpe: Type, posOpt: Option[Position])
                                                           (using tcCtx: TypeCheckingContext): Unit = {
    tpe.shape match {
      case RegionType if !CaptureDescriptors.singletonSetOfRoot.subcaptureOf(tpe.captureDescriptor) =>
        reportError(
          s"type $tpe is not inhabited, as a region always captures the root capability",
          posOpt,
          isWarning = true
        )
      case primType: PrimitiveTypeShape if primType != RegionType && !tpe.captureDescriptor.isEmpty =>
        reportError(
          s"capture set is useless, as values of type ${tpe.shape} never capture capabilities",
          posOpt,
          isWarning = true
        )
      case _ => ()
    }
  }

  private def warnOnNonCapAndRedundantCaptures(captureDescrTree: CaptureDescrTree)
                                              (using tcCtx: TypeCheckingContext): Unit = {
    captureDescrTree match {
      case ExplicitCaptureSetTree(capturedExpressions) =>
        var foundNotCap = false
        capturedExpressions.foreach { captExpr =>
          if (captExpr.getType.captureDescriptor.isEmpty) {
            reportError("captured expression is not a capability", captExpr.getPosition, isWarning = true)
            foundNotCap = true
          }
        }
        if (!foundNotCap) {
          // do not try to find redundant capabilities if we have already found a problem on this capture set
          captureDescrTree.getResolvedDescrOpt.foreach {
            case CaptureSet(set) =>
              set.find(c => c.isCoveredBy(CaptureSet(set - c)))
                .foreach { capability =>
                  reportError(
                    s"redundant capability: $capability is already covered by the rest of the capture set",
                    captureDescrTree.getPosition,
                    isWarning = true
                  )
                }
            case _ => ()
          }
        }
      case _ => ()
    }
  }

  private def checkTypeShape(typeShapeTree: TypeShapeTree, idsAreFields: Boolean)
                            (using tcCtx: TypeCheckingContext, langMode: LanguageMode): TypeShape = typeShapeTree match {
    case ArrayTypeShapeTree(elemTypeTree, isModifiable) =>
      val elemType = checkType(elemTypeTree, idsAreFields)
      typeShouldNotCaptureRoot(elemType, "array elements are not allowed to capture the root capability",
        tcCtx, elemTypeTree.getPosition)
      ArrayTypeShape(elemType, isModifiable)
    case castTargetTypeShapeTree: CastTargetTypeShapeTree =>
      checkCastTargetTypeShape(castTargetTypeShapeTree)
  }

  private def checkCastTargetTypeShape(castTargetTypeShapeTree: CastTargetTypeShapeTree)
                                      (using tcCtx: TypeCheckingContext, langMode: LanguageMode): CastTargetTypeShape = {
    castTargetTypeShapeTree match {
      case PrimitiveTypeShapeTree(primitiveType) =>
        if (primitiveType == RegionType && langMode == OcapDisabled) {
          featureIsNotAllowedIfOcapDisabled("region type", castTargetTypeShapeTree.getPosition)
        }
        primitiveType
      case NamedTypeShapeTree(name) =>
        if (!tcCtx.knowsUserDefType(name)) {
          reportError(s"unknown: $name", castTargetTypeShapeTree.getPosition)
        }
        NamedTypeShape(name)
    }
  }

  private def checkCaptureDescr(captureDescrTree: CaptureDescrTree, idsAreFields: Boolean)
                               (using tcCtx: TypeCheckingContext, langMode: LanguageMode): CaptureDescriptor = {
    val captureDescr = captureDescrTree match {
      case explicitCaptureSetTree: ExplicitCaptureSetTree =>
        checkCaptureSet(explicitCaptureSetTree, idsAreFields)
      case ImplicitRootCaptureSetTree() => CaptureSet.singletonOfRoot
      case MarkTree() => Mark
    }
    captureDescrTree.setResolvedDescr(captureDescr)
    captureDescr
  }

  private def checkCaptureSet(explicitCaptureSetTree: ExplicitCaptureSetTree, idsAreFields: Boolean)
                             (using tcCtx: TypeCheckingContext, langMode: LanguageMode): CaptureSet = {
    CaptureSet(explicitCaptureSetTree.capturedExpressions.flatMap { expr =>
      val exprType = checkExpr(expr)
      if exprType.captureDescriptor.isEmpty then None
      else convertToCapturable(expr, Some(errorReporter), idsAreFields)(using tcCtx)
    }.toSet)
  }

  private def convertToCapturable(expr: Expr, erOpt: Option[ErrorReporter], idsAreFields: Boolean)
                                 (using tcCtx: TypeCheckingContext): Option[ConcreteCapturable] = {

    def maybeReportError(msg: String): None.type = {
      erOpt.foreach(_.push(Err(TypeChecking, msg, expr.getPosition)))
      None
    }

    expr match {
      case VariableRef(name) =>
        tcCtx.getLocalOnly(name) match {
          case None => None // do not report, an error will be reported checkExpr anyways
          case Some(localInfo) if localInfo.isReassignable =>
            maybeReportError(s"'$name is not capturable, as it is a var")
          case Some(localInfo) if idsAreFields =>
            Some(MePath.dot(name))
          case Some(localInfo) =>
            Some(IdPath(name))
        }
      case MeRef() => Some(MePath)
      case PackageRef(pkgName) => Some(CapPackage(pkgName))
      case DeviceRef(device) => Some(CapDevice(device))
      case Select(lhs, selected) =>
        convertToCapturable(lhs, erOpt, idsAreFields).flatMap {
          case lhsPath: Path => {
            lhs.getType.shape match {
              case NamedTypeShape(lhsTypeId) =>
                tcCtx.resolveTypeAs[SelectableSig & ConstructibleSig](lhsTypeId).flatMap { lhsTypeSig =>
                  lhsTypeSig.params.get(selected).flatMap { fieldInfo =>
                    if fieldInfo.isReassignable
                    then maybeReportError(s"field $selected of $lhsTypeId is not capturable, as it is reassignable")
                    else Some(lhsPath.dot(selected))
                  }
                }
              case lhsType => None
            }
          }
          case _ => None
        }
      case _ => maybeReportError(s"expression is not capturable")
    }
  }

  private def checkStat(statement: Statement)
                       (using tcCtx: TypeCheckingContext, langMode: LanguageMode, expRetType: Type): Unit = statement match {

    case expr: Expr => checkExpr(expr)

    case Block(stats) =>
      val newCtx = tcCtx.copied
      for stat <- stats do {
        checkStat(stat)(using newCtx)
      }
      newCtx.writeLocalsRelatedWarnings(errorReporter)

    case localDef@LocalDef(localName, optTypeAnnotTree, rhsOpt, isReassignable) =>
      val inferredTypeOpt = rhsOpt.map(checkExpr)
      val optAnnotType = optTypeAnnotTree.map { typeAnnotTree =>
        val annotType = checkType(typeAnnotTree, idsAreFields = false)(using tcCtx)
        inferredTypeOpt.foreach { inferredType =>
          checkSubtypingConstraint(annotType, inferredType, localDef.getPosition, "local definition")
        }
        annotType
      }
      val isUnionWithoutAnnot = optTypeAnnotTree.isEmpty && inferredTypeOpt.exists(_.isInstanceOf[UnionTypeShape])
      if (isUnionWithoutAnnot) {
        reportError(s"Please provide an explicit type for $localName", localDef.getPosition)
      }
      val actualType =
        if isUnionWithoutAnnot then UndefinedTypeShape
        else optAnnotType.orElse(inferredTypeOpt).getOrElse {
          reportError(s"Please provide a type for uninitialized local $localName", localDef.getPosition)
        }
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
          val varType = typeOfLocalOrConst(name, tcCtx, varAssig.getPosition)
          checkSubtypingConstraint(varType, rhsType, varAssig.getPosition, "")
          varRef.setType(varType)
        case indexing@Indexing(indexed, _) =>
          checkExpr(indexing)
          val lhsType = exprMustBeIndexable(indexed.getType, tcCtx, varAssig.getPosition,
            mustUpdate = true, allowString = false)
          checkSubtypingConstraint(lhsType, rhsType, varAssig.getPosition, "")
        case select@Select(structExpr, selected) =>
          val structType = checkExpr(structExpr)
          val fieldType = checkFieldAccess(structExpr, selected, varAssig.getPosition, mustUpdateField = true)
          checkSubtypingConstraint(fieldType, rhsType, varAssig.getPosition, "")
          select.setType(fieldType)
        case _ =>
          reportError("syntax error: only variables, struct fields and array elements can be assigned", varAssig.getPosition)
      }
      checkIsAllowedAssignmentTarget(lhs)

    case varModif@VarModif(lhs, rhs, op) =>
      // no check for unused mut because no operator combinable with = can operate on mutable types
      val rhsType = checkExpr(rhs)
      lhs match {
        case varRef@VariableRef(name) =>
          tcCtx.varIsAssigned(name)
          val inPlaceModifiedType = typeOfLocalOrConst(name, tcCtx, varModif.getPosition)
          val operatorRetType = mustExistOperator(inPlaceModifiedType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedType, operatorRetType, varModif.getPosition, "")
          varRef.setType(inPlaceModifiedType)
        case indexing@Indexing(indexed, _) =>
          checkExpr(indexing)
          val inPlaceModifiedElemType = exprMustBeIndexable(indexed.getType, tcCtx, varModif.getPosition,
            mustUpdate = true, allowString = false)
          val operatorRetType = mustExistOperator(inPlaceModifiedElemType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedElemType, operatorRetType, varModif.getPosition, "")
        case select@Select(structExpr, selected) =>
          val structType = checkExpr(structExpr)
          val inPlaceModifiedFieldType = checkFieldAccess(structExpr, selected, varModif.getPosition, mustUpdateField = true)
          val operatorRetType = mustExistOperator(inPlaceModifiedFieldType, op, rhsType, varModif.getPosition)
          checkSubtypingConstraint(inPlaceModifiedFieldType, operatorRetType, varModif.getPosition, "")
          select.setType(inPlaceModifiedFieldType)
        case _ =>
          reportError("syntax error: only variables, struct fields and array elements can be assigned", varModif.getPosition)
      }
      checkIsAllowedAssignmentTarget(lhs)

    case ifThenElse@IfThenElse(cond, thenBr, elseBrOpt) =>
      val condType = checkExpr(cond)
      checkSubtypingConstraint(BoolType, condType, ifThenElse.getPosition, "if condition")
      val smartCasts = detectSmartCasts(cond, tcCtx)
      ifThenElse.setSmartCasts(smartCasts)
      checkStat(thenBr)(using tcCtx.copyWithSmartCasts(smartCasts))
      elseBrOpt.foreach(checkStat)

    case whileLoop@WhileLoop(cond, body) =>
      val condType = checkExpr(cond)
      checkSubtypingConstraint(BoolType, condType, whileLoop.getPosition, "while condition")
      val smartCasts = detectSmartCasts(cond, tcCtx)
      checkStat(body)(using tcCtx.copyWithSmartCasts(smartCasts))

    case forLoop@ForLoop(initStats, cond, stepStats, body) =>
      val newCtx = tcCtx.copied
      initStats.foreach(checkStat(_)(using newCtx))
      val condType = checkExpr(cond)(using newCtx)
      checkSubtypingConstraint(BoolType, condType, forLoop.getPosition, "for loop condition")
      val smartCasts = detectSmartCasts(cond, tcCtx)
      val smartCastsAwareCtx = newCtx.copyWithSmartCasts(smartCasts)
      stepStats.foreach(checkStat(_)(using smartCastsAwareCtx))
      checkStat(body)(using smartCastsAwareCtx)
      newCtx.writeLocalsRelatedWarnings(errorReporter)

    case retStat@ReturnStat(valueOpt) =>
      valueOpt.foreach { value =>
        val retType = checkExpr(value)
        checkSubtypingConstraint(expRetType, retType, retStat.getPosition, "returned value")
      }
      if (expRetType == NothingType) {
        reportError(s"unexpected return in function returning $NothingType", retStat.getPosition)
      } else if (expRetType != VoidType && valueOpt.isEmpty) {
        reportError("expected an expression after return", retStat.getPosition)
      } else if (expRetType == VoidType && valueOpt.isDefined) {
        reportError("unexpected expression after return", valueOpt.get.getPosition)
      }

    case panicStat@PanicStat(msg) =>
      val msgType = checkExpr(msg)
      checkSubtypingConstraint(StringType, msgType, panicStat.getPosition, "panic")

    // TODO writes tests that try to escape a value from an enclosure
    case restr@RestrictedStat(captureSetTree, body) =>
      featureIsNotAllowedIfOcapDisabled("restricted", restr.getPosition)
      if (tcCtx.environment.insideEnclosure) {
        reportError("restricted blocks are not allowed inside enclosures", restr.getPosition)
      }
      val captureSet = checkCaptureSet(captureSetTree, idsAreFields = false)
      val innerCtx = tcCtx.copyWithEnvironment(RestrictedEnvir(captureSet, _))
      checkStat(body)(using innerCtx)

    case encl@EnclosedStat(captureSetTree, body) =>
      featureIsNotAllowedIfOcapDisabled("enclosures", encl.getPosition)
      val captureSet = checkCaptureSet(captureSetTree, idsAreFields = false)
      for (capability <- captureSetTree.capturedExpressions) {
        val isRegion = capability.getTypeShape == RegionType
        val isDevice = capability.isInstanceOf[DeviceRef]
        val isAllowed = isRegion || isDevice
        if (!isAllowed) {
          reportError(
            s"only regions and devices may appear in the capture set of an enclosed block",
            capability.getPosition
          )
        }
      }
      val innerCtx = tcCtx.copyWithEnvironment(EnclosedEnvir(_))
      checkStat(body)(using innerCtx)
  }

  private def checkLiteralExpr(literal: Literal): Type = literal match {
    case _: IntLit => IntType
    case _: DoubleLit => DoubleType
    case _: CharLit => CharType
    case _: BoolLit => BoolType
    case _: StringLit => StringType
  }

  private def checkExpr(expr: Expr)(using tcCtx: TypeCheckingContext, langMode: LanguageMode): Type = {
    val tpe = shapeOnlyIfOcapDisabled(expr match {

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
        if (!tcCtx.allowedPackages.contains(packageName)) {
          reportError(s"package $packageName has not been imported", pkg.getPosition)
        }
        tcCtx.resolveType(packageName) match {
          case Some(packageSignature: PackageSignature) => packageSignature.getNonSubstitutedType
          case Some(_) => reportError(s"$packageName is not a package and is thus not allowed here", pkg.getPosition)
          case None => reportError(s"not found: $packageName", pkg.getPosition)
        }

      case devRef@DeviceRef(device) =>
        if (!tcCtx.allowedDevices.contains(device)) {
          reportError(s"device $device has not been imported", devRef.getPosition)
        }
        device.tpe

      case call@Call(None, funName, args) =>
        checkFunCall(call, IntrinsicsPackageId, fallbackOwnerOpt = Some(tcCtx.meTypeId))

      case call@Call(Some(receiver), funName, args) =>
        checkExpr(receiver).shape match {
          case namedType: NamedTypeShape =>
            checkFunCall(call, namedType.typeName, None)
          case recType =>
            reportError(s"expected a module or package type, found $recType", receiver.getPosition)
        }

      case indexing@Indexing(indexed, arg) =>
        val indexedType = checkExpr(indexed)
        val argType = checkExpr(arg)
        checkSubtypingConstraint(IntType, argType, indexing.getPosition, "array index")
        val elemType = exprMustBeIndexable(indexed.getType, tcCtx, indexed.getPosition, mustUpdate = false, allowString = true)
        checkUnboxedType(elemType, indexing.getPosition)
        elemType

      case arrayInit@ArrayInit(regionOpt, elemTypeTree, size) =>
        if (elemTypeTree == VoidType || elemTypeTree == NothingType) {
          reportError(s"array cannot have element type $elemTypeTree", arrayInit.getPosition)
        }
        val sizeType = checkExpr(size)
        checkSubtypingConstraint(IntType, sizeType, size.getPosition, "array size")
        size match {
          case IntLit(value) if value < 0 =>
            reportError("array size should be nonnegative", size.getPosition)
          case _ => ()
        }
        val elemType = checkType(elemTypeTree, idsAreFields = false)
        requireRegionIffOcapEnabled(regionOpt, arrayInit.getPosition, isMutableObj = true)
        ArrayTypeShape(elemType, modifiable = true) ^ computeCaptures(regionOpt.toList)

      case filledArrayInit@FilledArrayInit(Nil, regionOpt) =>
        reportError("cannot infer type of empty array, use 'arr <type>[0]' instead", filledArrayInit.getPosition)

      case filledArrayInit@FilledArrayInit(arrayElems, regionOpt) =>
        requireRegionIffOcapEnabled(regionOpt, filledArrayInit.getPosition, isMutableObj = regionOpt.isDefined)
        val types = arrayElems.map(checkExpr)
        computeJoinOf(types.toSet, tcCtx) match {
          case Some(elemsJoin) =>
            ArrayTypeShape(elemsJoin, modifiable = regionOpt.isDefined) ^ computeCaptures(regionOpt.toList)
          case None =>
            reportError("cannot infer array type", filledArrayInit.getPosition)
        }

      case instantiation@StructOrModuleInstantiation(regionOpt, tid, args) =>
        regionOpt.foreach(checkExpr)
        tcCtx.resolveType(tid) match {
          case Some(structSig: StructSignature) if structSig.isInterface =>
            reportError(
              s"cannot instantiate interface $tid",
              instantiation.getPosition
            )
          case Some(structSig: StructSignature) =>
            if (langMode == OcapEnabled && structSig.isShallowMutable && regionOpt.isEmpty) {
              reportError(
                s"cannot instantiate '$tid' without providing a region, since at least one of its fields is reassignable",
                instantiation.getPosition
              )
            } else if (langMode == OcapEnabled && !structSig.isShallowMutable && regionOpt.isDefined) {
              reportError(
                s"providing a region is not necessary here, as ${structSig.id} does not have reassignable fields",
                regionOpt.get.getPosition,
                isWarning = true
              )
            }
            checkLangModeCompatibility(s"constructor of struct $tid", structSig.languageMode, instantiation.getPosition)
            checkCallArgs(structSig, structSig.voidInitMethodSig, None, args, instantiation.getPosition)
            NamedTypeShape(tid) ^ computeCaptures(args ++ regionOpt)
          case Some(moduleSig: ModuleSignature) =>
            checkLangModeCompatibility(s"constructor of module $tid", moduleSig.languageMode, instantiation.getPosition)
            checkCallArgs(moduleSig, moduleSig.voidInitMethodSig, None, args, instantiation.getPosition)
            NamedTypeShape(tid) ^ computeCaptures(args ++ regionOpt)
          case _ => reportError(s"not found: structure or module '$tid'", instantiation.getPosition)
        }

      case regCreation@RegionCreation() =>
        featureIsNotAllowedIfOcapDisabled("regions", regCreation.getPosition)
        RegionType ^ CaptureSet.singletonOfRoot

      case unaryOp@UnaryOp(operator, operand) =>
        val operandType = checkExpr(operand)
        val operandTypeShape = operandType.shape
        if (operator == Sharp) {
          if (operandTypeShape.isInstanceOf[ArrayTypeShape] || operandTypeShape == StringType) {
            IntType
          } else {
            reportError(s"operator # can only be applied to arrays and strings, found '$operandType'", unaryOp.getPosition)
          }
        } else {
          unaryOperatorSignatureFor(operator, operandType.shape) match {
            case Some(sig) => sig.retType
            case None =>
              reportError(s"no definition of operator '$operator' found for operand '$operandType'", unaryOp.getPosition)
          }
        }

      case binOp@BinaryOp(lhs, operator, rhs) =>
        // no check for unused mut because no binary operator requires mutability on its arguments
        val lhsType = checkExpr(lhs)
        val smartCasts = if operator == Operator.And then detectSmartCasts(lhs, tcCtx) else Map.empty
        val rhsType = checkExpr(rhs)(using tcCtx.copyWithSmartCasts(smartCasts))
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
        checkFieldAccess(lhs, selected, select.getPosition, mustUpdateField = false)

      case ternary@Ternary(cond, thenBr, elseBr) =>
        val condType = checkExpr(cond)
        checkSubtypingConstraint(BoolType, condType, ternary.getPosition, "ternary operator condition")
        val smartCasts = detectSmartCasts(cond, tcCtx)
        ternary.setSmartCasts(smartCasts)
        val thenType = checkExpr(thenBr)(using tcCtx.copyWithSmartCasts(smartCasts))
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
        val targetType = checkCastTargetTypeShape(typeTree)
        val castTypeShape = if (exprType.shape.subtypeOf(targetType)) {
          reportError(s"useless conversion: '${exprType.shape}' --> '$targetType'", cast.getPosition, isWarning = true)
          cast.markTransparent()
          targetType
        } else if (TypeConversion.conversionFor(exprType.shape, targetType).isDefined) {
          targetType
        } else {
          structCastResult(exprType.shape, targetType, tcCtx).getOrElse {
            reportError(s"cannot cast '${expr.getType}' to '$targetType'", cast.getPosition)
          }
        }
        castTypeShape ^ exprType.captureDescriptor

      case typeTest@TypeTest(expr, typeTree) =>
        val exprType = checkExpr(expr)
        val targetType = checkCastTargetTypeShape(typeTree)
        if (exprType.shape.subtypeOf(targetType)) {
          reportError(s"test will always be true, as '${exprType.shape}' is a subtype of '$targetType'",
            typeTest.getPosition, isWarning = true)
        } else if (structCastResult(exprType.shape, targetType, tcCtx).isEmpty) {
          reportError(s"cannot check expression of type '${exprType.shape}' against type '$targetType'", typeTest.getPosition)
        }
        BoolType

      case _: Sequence => throw AssertionError("should not happen, as Sequences are produced by the desugaring phase")
    })
    expr.setType(tpe)
    checkIsAllowedInCurrentEnvir(expr)
    tpe
  }

  private def checkIsAllowedInCurrentEnvir(expr: Expr)(using tcCtx: TypeCheckingContext): Unit = {

    def performCheck(capt: Capturable): Unit = {
      if (!tcCtx.environment.allowsCapturable(capt)) {
        reportError(s"$capt is not allowed in the current environment", expr.getPosition)
      }
    }

    expr match {
      case VariableRef(name) => performCheck(IdPath(name))
      case MeRef() => performCheck(MePath)
      case PackageRef(pkgName) => performCheck(CapPackage(pkgName))
      case DeviceRef(device) => performCheck(CapDevice(device))
      case _ => ()
    }
  }

  private def checkIsAllowedAssignmentTarget(expr: Expr)(using tcCtx: TypeCheckingContext): Unit = {
    val cd = expr.getType.captureDescriptor
    if (tcCtx.environment.insideEnclosure && cd != Mark && cd != CaptureSet.empty) {
      expr match {
        // allow assignments to variables that have been declared in the current environment
        case VariableRef(name) if tcCtx.getLocalOnly(name).exists(_.declaringEnvir == tcCtx.environment) => ()
        case _ =>
          reportError("illegal assignment: cannot prove that no value of a marked type is being leaked", expr.getPosition)
      }
    }
  }

  private def requireRegionIffOcapEnabled(regionOpt: Option[Expr], posOpt: Option[Position], isMutableObj: Boolean)
                                         (using ctx: TypeCheckingContext, langMode: LanguageMode): Unit = {
    regionOpt.foreach(checkExpr)
    (regionOpt, langMode) match {
      case (None, OcapDisabled) => ()
      case (None, OcapEnabled) if isMutableObj =>
        reportError("expected a region", posOpt)
      case (None, OcapEnabled) => ()
      case (Some(_), OcapDisabled) =>
        reportError("unexpected region, as ocap is disabled for this file", posOpt)
      case (Some(region), OcapEnabled) =>
        val regType = region.getType
        checkSubtypingConstraint(PrimitiveTypeShape.RegionType ^ CaptureSet.singletonOfRoot, regType, region.getPosition, "region")
    }
  }

  private def unaryOperatorSignatureFor(operator: Operator, operand: TypeShape)
                                       (using TypeCheckingContext, LanguageMode): Option[UnaryOpSignature] = {
    unaryOperators.find {
      case UnaryOpSignature(op, operandType, _) =>
        operator == op && operand.subtypeOf(operandType)
    }
  }

  private def binaryOperatorSigFor(left: TypeShape, operator: Operator, right: TypeShape)
                                  (using TypeCheckingContext, LanguageMode): Option[BinaryOpSignature] = {
    binaryOperators.find {
      case BinaryOpSignature(leftOperandType, op, rightOperandType, _) =>
        left.subtypeOf(leftOperandType) && op == operator && right.subtypeOf(rightOperandType)
    }
  }

  private def checkFunCall(
                            call: Call,
                            owner: TypeIdentifier,
                            fallbackOwnerOpt: Option[TypeIdentifier]
                          )(using tcCtx: TypeCheckingContext, langMode: LanguageMode): Type = {
    val funName = call.function
    val args = call.args
    val pos = call.getPosition
    tcCtx.resolveFunc(owner, funName) match {
      case FunctionFound(typeSig, funSig) =>
        call.setResolvedSig(funSig)
        call.cacheMeType(tcCtx.meType)
        val someReceiver = call.receiverOpt.orElse {
          if typeSig.id == IntrinsicsPackageId then None
          else Some(MeRef().setType(tcCtx.meType))
        }
        checkLangModeCompatibility(s"function $funName", funSig.languageMode, call.getPosition)
        checkCallArgs(typeSig, funSig, someReceiver, args, pos)
      case ModuleNotFound =>
        args.foreach(checkExpr)
        reportError(s"not found: package or module $owner", pos)
      case FunctionNotFound(typeSig) =>
        fallbackOwnerOpt map { fallbackOwner =>
          checkFunCall(call, fallbackOwner, None)
        } getOrElse {
          args.foreach(checkExpr)
          reportError(s"function not found: '$funName' in '${typeSig.id}'", pos)
        }
    }
  }

  private def checkUnboxedType(tpe: Type, posOpt: Option[Position])(using tcCtx: TypeCheckingContext): Unit = tpe match {
    case CapturingType(shape, captureDescriptor) =>
      checkUnboxedType(shape, posOpt)
      checkUnboxedCaptureDescr(captureDescriptor, posOpt)
    case ArrayTypeShape(elemType, modifiable) =>
      checkUnboxedType(elemType, posOpt)
    case UnionTypeShape(unitedTypes) =>
      unitedTypes.foreach(checkUnboxedType(_, posOpt))
    case _: (UndefinedTypeShape.type | PrimitiveTypeShape | NamedTypeShape) => ()
  }

  private def checkUnboxedCaptureDescr(captureDescriptor: CaptureDescriptor, posOpt: Option[Position])
                                      (using tcCtx: TypeCheckingContext): Unit = {
    captureDescriptor match
      case CaptureDescriptors.Mark =>
        if (!tcCtx.environment.insideEnclosure) {
          reportError(s"type cannot be unboxed, as it captures '$Mark'", posOpt)
        }
      case CaptureSet(set) =>
        set.foreach {
          case path: Path =>
            path.root match {
              case IdPath(id) =>
                if (tcCtx.getLocalOnly(id).isEmpty) {
                  reportError(s"type cannot be unboxed: '$id' is not defined in current scope", posOpt)
                }
              case MePath => ()
            }
          case RootCapability =>
            reportError(s"type cannot be unboxed, as it captures the root capability", posOpt)
          case _: (CapPackage | CapDevice) => ()
        }
  }

  private def structCastResult(srcType: TypeShape, destType: TypeShape, ctx: TypeCheckingContext)
                              (using TypeCheckingContext, LanguageMode): Option[NamedTypeShape] = {
    (srcType, destType) match {
      case (srcType: NamedTypeShape, destType: NamedTypeShape)
        if destType.subtypeOf(srcType) || srcType.subtypeOf(destType)
      => Some(destType)
      case _ => None
    }
  }

  private def checkLangModeCompatibility(funDescription: String, calleeLangMode: LanguageMode, callPosOpt: Option[Position])
                                        (using tcCtx: TypeCheckingContext, callerLangMode: LanguageMode): Unit = {
    if (callerLangMode.isOcapEnabled && calleeLangMode.isOcapDisabled && !tcCtx.environment.insideEnclosure){
      reportError(s"cannot call $funDescription in an unchecked environment, please use an enclosed block", callPosOpt)
    }
  }

  private def checkCallArgs(
                             funOwnerSig: TypeSignature,
                             funSig: FunctionSignature,
                             receiverOpt: Option[Expr],
                             args: List[Expr],
                             callPos: Option[Position]
                           )(using callerCtx: TypeCheckingContext, callerLangMode: LanguageMode): Type = {
    val expTypesIter = funSig.argsForMode(callerLangMode).iterator
    val argsIter = args.iterator
    val calleeCtx = TypeCheckingContext(
      callerCtx.analysisContext,
      RootEnvir,
      meTypeId = funOwnerSig.id,
      meCaptureDescr = funOwnerSig.getNonSubstitutedCaptureDescr,
      callerCtx.analysisContext.packages.keySet,
      Device.values.toSet
    )
    val substitutor = PathsSubstitutor(calleeCtx, errorReporter)
    for {
      receiver <- receiverOpt
      receiverPath <- convertToCapturable(receiver, erOpt = None, idsAreFields = false)
    } do {
      substitutor(MePath) = receiverPath
    }
    var errorFound = false
    while (expTypesIter.hasNext && argsIter.hasNext && !errorFound) {
      val (paramNameOpt, expTypeRaw) = expTypesIter.next()
      val arg = argsIter.next()
      val expTypeSubst = substitutor.subst(expTypeRaw, arg.getPosition)
      val actType = checkExpr(arg)
      if (!actType.subtypeOf(expTypeSubst)) {
        reportError(s"expected '$expTypeSubst', found '$actType'", arg.getPosition)
        errorFound = true
      }
      for {
        paramName <- paramNameOpt
        argPath <- convertToCapturable(arg, erOpt = None, idsAreFields = false)
      } do {
        substitutor(IdPath(paramName)) = argPath
      }
    }
    if (expTypesIter.hasNext && !errorFound) {
      reportError(s"expected argument of type '${expTypesIter.next()._2}', found end of arguments list", callPos)
    }
    if (argsIter.hasNext && !errorFound) {
      val arg = argsIter.next()
      reportError(s"expected end of arguments list, found argument of type '${checkExpr(arg)}'", arg.getPosition)
    }
    for arg <- argsIter do {
      checkExpr(arg)
    }
    substitutor.subst(funSig.retTypeForMode(callerLangMode), callPos)
  }

  private def computeCaptures(captured: List[Expr])(using TypeCheckingContext): CaptureDescriptor =
    CaptureDescriptors.unionOf(captured.map(minimalCaptureSetFor))

  private def detectSmartCasts(expr: Expr, ctx: TypeCheckingContext): Map[FunOrVarId, CastTargetTypeShape] = {
    expr match {
      case BinaryOp(lhs, Operator.And, rhs) =>
        // concat order is reversed because we want to keep the leftmost type test in case of conflict
        detectSmartCasts(rhs, ctx) ++ detectSmartCasts(lhs, ctx)
      case TypeTest(VariableRef(varName), typeTree) if ctx.getLocalOnly(varName).exists(!_.isReassignable) =>
        typeTree.getResolvedTypeOpt.map { tpe =>
          Map(varName -> tpe)
        }.getOrElse(Map.empty)
      case _ => Map.empty
    }
  }

  /**
   * @param alwaysStopped indicates whether the control-flow can reach the end of the considered construct without
   *                      encountering an instruction that terminates the function (`return` or `panic`)
   */
  private case class EndStatus(alwaysStopped: Boolean)

  private def typeOfLocalOrConst(name: FunOrVarId, ctx: TypeCheckingContext, posOpt: Option[Position]): Type = {
    ctx.getLocalOrConst(name) match
      case None =>
        reportError(s"unknown: $name", posOpt)
      case Some(info) => info.tpe
  }

  private def exprMustBeIndexable(
                                   exprType: Type,
                                   ctx: TypeCheckingContext,
                                   posOpt: Option[Position],
                                   mustUpdate: Boolean,
                                   allowString: Boolean
                                 ): Type = {
    require(!(mustUpdate && allowString))
    exprType.shape match
      case ArrayTypeShape(elemType, modifiable) =>
        if (mustUpdate && !modifiable) {
          reportError("update impossible: missing modification privileges on array", posOpt)
        }
        elemType
      case StringType if allowString => CharType
      case _ =>
        val expectedDescr = if allowString then "an array or a string" else "an array"
        reportError(s"expected $expectedDescr, found '$exprType'", posOpt)
  }

  private def mustExistOperator(lhsType: Type, operator: Operator, rhsType: Type, position: Option[Position])
                               (using TypeCheckingContext, LanguageMode): Type = {
    binaryOperatorSigFor(lhsType.shape, operator, rhsType.shape) match {
      case Some(sig) => sig.retType
      case None =>
        reportError(s"no definition of operator '$operator' found for operands '$lhsType' and '$rhsType'", position)
    }
  }

  private def checkFieldAccess(
                                receiver: Expr,
                                fieldName: FunOrVarId,
                                posOpt: Option[Position],
                                mustUpdateField: Boolean
                              )(using callerCtx: TypeCheckingContext, langMode: LanguageMode): Type = {

    val receiverCaptOpt = convertToCapturable(receiver, erOpt = None, idsAreFields = false)

    def performSubstIfApplicable(rawType: Type, structOrModuleSignature: ConstructibleSig): Type = {
      val calleeCtx = TypeCheckingContext(
        callerCtx.analysisContext,
        RootEnvir,
        meTypeId = structOrModuleSignature.id,
        meCaptureDescr = structOrModuleSignature.getNonSubstitutedCaptureDescr,
        callerCtx.analysisContext.packages.keySet,
        Device.values.toSet
      )
      for ((paramName, fieldInfo) <- structOrModuleSignature.params) {
        calleeCtx.addLocal(paramName, fieldInfo.tpe, None, false, true, () => (), () => ())
      }
      val substitutor = PathsSubstitutor(calleeCtx, errorReporter)
      receiverCaptOpt.foreach { recvCapt =>
        substitutor(MePath) = recvCapt
      }
      substitutor.subst(rawType, receiver.getPosition)
    }

    val exprType = receiver.getType
    exprType.shape match {
      case NamedTypeShape(typeName) =>
        callerCtx.resolveType(typeName) match {
          case Some(structSig@StructSignature(_, fields, _, _, structLangMode)) if fields.contains(fieldName) =>
            val FieldInfo(fieldType, fieldIsReassig, _) = fields.apply(fieldName)
            val missingFieldMutability = mustUpdateField && !fieldIsReassig
            if (missingFieldMutability) {
              reportError(s"cannot update immutable field '$fieldName'", posOpt)
            }
            performSubstIfApplicable(fieldType, structSig)
          case Some(modSig@ModuleSignature(_, paramImports, _, _, _)) if paramImports.contains(fieldName) =>
            if (!receiver.isInstanceOf[MeRef]) {
              reportError(s"references to module imports are only allowed when using the '${Keyword.Me}' keyword", posOpt)
            }
            if (mustUpdateField) {
              reportError("cannot update module field", posOpt)
            }
            performSubstIfApplicable(paramImports.apply(fieldName), modSig)
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
                                        msgPrefix: String
                                      )(using ctx: TypeCheckingContext, langMode: LanguageMode): Unit = {
    if (expected != UndefinedTypeShape && actual != UndefinedTypeShape && !actual.subtypeOf(expected)) {
      val fullprefix = if msgPrefix == "" then "" else (msgPrefix ++ ": ")
      reportError(fullprefix ++ s"expected '$expected', found '$actual'", posOpt)
    }
  }

  private def computeJoinOf(types: Set[Type], ctx: TypeCheckingContext)(using LanguageMode): Option[Type] = {
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
          val cd = CaptureDescriptors.unionOf(types.map(_.captureDescriptor))
          UnionTypeShape(types.map(_.shape)) ^ cd
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

  private def minimalCaptureSetFor(expr: Expr)(using TypeCheckingContext): CaptureDescriptor = {
    convertToCapturable(expr, erOpt = None, idsAreFields = false) map { path =>
      CaptureSet(path)
    } getOrElse {
      expr.getType.captureDescriptor
    }
  }

  private def shapeOnlyIfOcapDisabled(tpe: Type)(using langMode: LanguageMode): Type = {
    if langMode == OcapEnabled then tpe else tpe.shape
  }

  private def featureIsNotAllowedIfOcapDisabled(featureDescr: String, posOpt: Option[Position])(using langMode: LanguageMode): Unit = {
    if (langMode == OcapDisabled) {
      reportError(featureDescr + " should not be used, as ocap is disabled in the current file", posOpt)
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
