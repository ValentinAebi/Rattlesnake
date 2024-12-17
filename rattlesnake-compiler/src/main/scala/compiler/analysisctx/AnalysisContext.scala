package compiler.analysisctx

import compiler.analysisctx.AnalysisContext.{FunctionFound, FunctionNotFound, MethodResolutionResult, ModuleNotFound}
import compiler.irs.Asts.*
import compiler.pipeline.CompilationStep.ContextCreation
import compiler.reporting.Errors.{Err, ErrorReporter}
import compiler.reporting.Position
import compiler.typechecker.SubtypeRelation.subtypeOf
import compiler.typechecker.{RootEnvir, TypeCheckingContext}
import identifiers.{FunOrVarId, IntrinsicsPackageId, TypeIdentifier}
import lang.*
import lang.Capturables.*
import lang.CaptureDescriptors.*
import lang.LanguageMode.*
import lang.Types.*
import lang.Types.PrimitiveTypeShape.{NothingType, VoidType}

import scala.collection.mutable
import scala.reflect.ClassTag

final case class AnalysisContext private(
                                          modules: Map[TypeIdentifier, ModuleSignature],
                                          packages: Map[TypeIdentifier, PackageSignature],
                                          structs: Map[TypeIdentifier, StructSignature],
                                          constants: Map[FunOrVarId, Type]
                                        ) {

  def knowsUserDefType(tid: TypeIdentifier): Boolean =
    knowsPackageOrModule(tid) || knowsStruct(tid)

  def knowsPackageOrModule(tid: TypeIdentifier): Boolean = knowsPackage(tid) || knowsModule(tid)

  def knowsPackage(tid: TypeIdentifier): Boolean = packages.contains(tid)

  def knowsModule(tid: TypeIdentifier): Boolean = modules.contains(tid)

  def knowsStruct(tid: TypeIdentifier): Boolean = structs.contains(tid)

  def resolveType(tid: TypeIdentifier): Option[TypeSignature] =
    structs.get(tid)
      .orElse(modules.get(tid))
      .orElse(packages.get(tid))

  def resolveTypeAs[S <: TypeSignature : ClassTag](tid: TypeIdentifier): Option[S] = resolveType(tid) match {
    case Some(s: S) => Some(s)
    case _ => None
  }

  def resolveFunc(owner: TypeIdentifier, methodName: FunOrVarId): MethodResolutionResult = {
    resolveTypeAs[FunctionsProviderSig](owner)
      .map { ownerSig =>
        ownerSig.functions.get(methodName) map { sig =>
          FunctionFound(ownerSig, sig)
        } getOrElse FunctionNotFound(ownerSig)
      } getOrElse ModuleNotFound
  }

}

object AnalysisContext {

  sealed trait MethodResolutionResult {
    def getFunSigOrThrow(): FunctionSignature
  }

  final case class FunctionFound(typeSig: FunctionsProviderSig, funSig: FunctionSignature) extends MethodResolutionResult {
    override def getFunSigOrThrow(): FunctionSignature = funSig
  }

  object ModuleNotFound extends MethodResolutionResult {
    override def getFunSigOrThrow(): FunctionSignature = throw new NoSuchElementException()
  }

  final case class FunctionNotFound(typeSig: FunctionsProviderSig) extends MethodResolutionResult {
    override def getFunSigOrThrow(): FunctionSignature = throw new NoSuchElementException()
  }

  final class Builder(errorReporter: ErrorReporter) {
    private val modules: mutable.Map[TypeIdentifier, ModuleSignature] = mutable.Map.empty
    private val packages: mutable.Map[TypeIdentifier, PackageSignature] = mutable.Map(
      IntrinsicsPackageId -> PackageSignature(
        id = IntrinsicsPackageId,
        importedPackages = mutable.LinkedHashSet.empty,
        importedDevices = mutable.LinkedHashSet.empty,
        functions = Intrinsics.intrinsics,
        languageMode = OcapEnabled
      )
    ) ++ Device.values.map { device =>
      device.typeName -> PackageSignature(
        id = device.typeName,
        importedPackages = mutable.LinkedHashSet.empty,
        importedDevices = mutable.LinkedHashSet.empty,
        functions = device.api.functions,
        languageMode = OcapEnabled
      )
    }
    private val structs: mutable.Map[TypeIdentifier, (StructSignature, Option[Position])] = mutable.Map.empty
    private val constants: mutable.Map[FunOrVarId, Type] = mutable.Map.empty

    def addModule(moduleDef: ModuleDef)(using langMode: LanguageMode): Unit = {
      if (langMode == OcapDisabled) {
        reportError("module in non-ocap file", moduleDef.getPosition)
      }
      val moduleName = moduleDef.moduleName
      if (checkTypeNotAlreadyDefined(moduleName, moduleDef.getPosition)) {
        val (importedModules, importedPackages, importedDevices) = analyzeImports(moduleDef)
        val functions = extractFunctions(moduleDef)
        val moduleSig = ModuleSignature(moduleName, importedModules, importedPackages, importedDevices, functions)
        modules.put(moduleName, moduleSig)
      }
    }

    def addPackage(packageDef: PackageDef)(using langMode: LanguageMode): Unit = {
      val packageName = packageDef.packageName
      if (checkTypeNotAlreadyDefined(packageName, packageDef.getPosition)) {
        val functions = extractFunctions(packageDef)
        val (implicitlyImportedPackages, implicitlyImportedDevices) = trackPackagesAndDevices(packageDef)
        val sig = PackageSignature(packageName, implicitlyImportedPackages, implicitlyImportedDevices, functions, langMode)
        packages.put(packageName, sig)
      }
    }

    def addStruct(structDef: StructDef)(using langMode: LanguageMode): Unit = {
      val name = structDef.structName
      if (checkTypeNotAlreadyDefined(name, structDef.getPosition)) {
        val fieldsMap = buildStructFieldsMap(structDef)
        val sig = StructSignature(name, fieldsMap, structDef.directSupertypes, structDef.isInterface, langMode)
        structs.put(name, (sig, structDef.getPosition))
      }
    }

    def addConstant(constDef: ConstDef): Unit = {
      import constDef.constName
      if (constants.contains(constName)) {
        reportError(s"redefinition of constant $constName", constDef.getPosition)
      } else {
        // type is already known since value is a Literal
        constants(constName) = constDef.value.getType
      }
    }

    def build(): AnalysisContext = {
      val builtCtx = new AnalysisContext(
        modules.toMap,
        packages.toMap,
        structs.map((tid, sigAndPos) => (tid, sigAndPos._1)).toMap,
        constants.toMap
      )
      checkFieldsTypeConformance(builtCtx)
      checkSubtypingCycles(builtCtx.structs)
      builtCtx
    }

    private def extractFunctions(modOrMkg: ModuleOrPackageDefTree)(using LanguageMode): Map[FunOrVarId, FunctionSignature] = {
      val functions = mutable.Map.empty[FunOrVarId, FunctionSignature]
      for funDef <- modOrMkg.functions do {
        val name = funDef.funName
        if (functions.contains(name)) {
          reportError(s"redefinition of function '$name'", funDef.getPosition)
        } else {
          val funSig = computeFunctionSig(funDef)
          functions.put(name, funSig)
          funDef.setSignature(funSig)
        }
      }
      functions.toMap
    }

    private def computeFunctionSig(funDef: FunDef)(using languageMode: LanguageMode): FunctionSignature = {
      val argsTypesB = List.newBuilder[(Option[FunOrVarId], Type)]
      for (Param(paramNameOpt, tpe, isReassignable) <- funDef.params) {
        argsTypesB.addOne(paramNameOpt, computeType(tpe, idsAreFields = false))
      }
      val retType = funDef.optRetType.map(computeType(_, idsAreFields = false)).getOrElse(VoidType)
      FunctionSignature(funDef.funName, argsTypesB.result(), retType, languageMode)
    }

    private def computeType(typeTree: TypeTree, idsAreFields: Boolean): Type = typeTree match {
      case CapturingTypeTree(typeShapeTree, captureDescr) =>
        CapturingType(computeTypeShape(typeShapeTree, idsAreFields), computeCaptureDescr(captureDescr, idsAreFields))
      case typeShapeTree: TypeShapeTree =>
        computeTypeShape(typeShapeTree, idsAreFields)
      case WrapperTypeTree(tpe) => tpe
    }

    private def computeTypeShape(typeShapeTree: TypeShapeTree, idsAreFields: Boolean): TypeShape = typeShapeTree match {
      case ArrayTypeShapeTree(elemType, isModifiable) => ArrayTypeShape(computeType(elemType, idsAreFields), isModifiable)
      case castTargetTypeShapeTree: CastTargetTypeShapeTree => computeCastTargetTypeShape(castTargetTypeShapeTree)
    }

    private def computeCastTargetTypeShape(castTargetTypeShapeTree: CastTargetTypeShapeTree): CastTargetTypeShape = {
      castTargetTypeShapeTree match
        case PrimitiveTypeShapeTree(primitiveType) => primitiveType
        case NamedTypeShapeTree(name) => NamedTypeShape(name)
    }

    private def computeCaptureDescr(cdTree: CaptureDescrTree, idsAreFields: Boolean): CaptureDescriptor = cdTree match {
      case ExplicitCaptureSetTree(capturedExpressions) =>
        // checks that the expression is indeed capturable are delayed to the type-checker
        CaptureSet(capturedExpressions.flatMap(mkCapturableOrFailSilently(_, idsAreFields)).toSet)
      case ImplicitRootCaptureSetTree() =>
        CaptureSet.singletonOfRoot
      case MarkTree() =>
        Mark
    }

    private def analyzeImports(moduleDef: ModuleDef) = {
      val importsMap = new mutable.LinkedHashMap[FunOrVarId, Type]()
      val packagesSet = new mutable.LinkedHashSet[TypeIdentifier]()
      val devicesSet = new mutable.LinkedHashSet[Device]()
      moduleDef.imports.foreach {
        case ParamImport(instanceId, moduleType) =>
          importsMap.put(instanceId, computeType(moduleType, idsAreFields = true))
        case PackageImport(packageId) =>
          packagesSet.add(packageId)
        case DeviceImport(device) =>
          devicesSet.add(device)
      }
      (importsMap, packagesSet, devicesSet)
    }

    private def trackPackagesAndDevices(pkg: PackageDef) = {
      val packages = mutable.LinkedHashSet[TypeIdentifier]()
      val devices = mutable.LinkedHashSet[Device]()
      pkg.collect {
        case PackageRef(packageName) =>
          packages.add(packageName)
        case DeviceRef(device) =>
          devices.add(device)
        case _ => ()
      }
      (packages, devices)
    }

    private def buildStructFieldsMap(structDef: StructDef)
                                    (using langMode: LanguageMode): mutable.LinkedHashMap[FunOrVarId, FieldInfo] = {
      val fieldsMap = new mutable.LinkedHashMap[FunOrVarId, FieldInfo]()
      for param <- structDef.fields do {
        param.paramNameOpt match {
          case None =>
            reportError("struct fields must be named", param.getPosition)
          case Some(paramName) =>
            val tpe = computeType(param.tpe, idsAreFields = true)
            if (checkIsNotVoidOrNothing(tpe, param.getPosition)) {
              if (!fieldsMap.contains(paramName)) {
                // the absence of duplicate fields will be reported by the type-checker
                fieldsMap.put(paramName, FieldInfo(tpe, param.isReassignable, langMode))
              }
            }
        }
      }
      fieldsMap
    }

    private def reportError(msg: String, posOpt: Option[Position]): Unit = {
      errorReporter.push(Err(ContextCreation, msg, posOpt))
    }

    private def checkTypeNotAlreadyDefined(tid: TypeIdentifier, posOpt: Option[Position]): Boolean = {
      val alreadyDefined = structs.contains(tid) || modules.contains(tid) || packages.contains(tid)
      if (alreadyDefined) {
        reportError(s"$tid is already defined", posOpt)
      }
      !alreadyDefined
    }

    private def checkIsNotVoidOrNothing(tpe: Type, posOpt: Option[Position]): Boolean = {
      val isVoidOrNothing = tpe == VoidType || tpe == NothingType
      if (isVoidOrNothing) {
        reportError(s"type $tpe is illegal at this place", posOpt)
      }
      !isVoidOrNothing
    }

    /**
     * Create a capturable for the given expression, without performing any check
     * (e.g. it won't reject a capture of a variable). These checks should be performed by the type-checker instead.
     * If the expression is obviously not capturable (e.g. a call), return None
     */
    private def mkCapturableOrFailSilently(expr: Expr, idsAreFields: Boolean): Option[Capturable] = expr match {
      case VariableRef(name) => Some(if idsAreFields then MePath.dot(name) else IdPath(name))
      case MeRef() => Some(MePath)
      case PackageRef(pkgName) => Some(CapPackage(pkgName))
      case DeviceRef(device) => Some(CapDevice(device))
      case Select(lhs, selected) =>
        mkCapturableOrFailSilently(lhs, idsAreFields).flatMap {
          case p: Path => Some(SelectPath(p, selected))
          case _ => None
        }
      case _ => None
    }

    private def checkFieldsTypeConformance(builtCtx: AnalysisContext): Unit = {
      for {
        (structId, (structSig, posOpt)) <- structs
        directSupertypeId <- structSig.directSupertypes
      } {
        structs.get(directSupertypeId) match {
          case Some((directSupertypeSig, _)) => {
            if (directSupertypeSig.isInterface) {
              // TODO carefully check what happens here
              // i.e. what elements each field is allowed to capture (fields of the super-/subtype)
              // and what me refers to
              val tcCtx = TypeCheckingContext(
                analysisContext = builtCtx,
                environment = RootEnvir,
                meTypeId = directSupertypeId,
                meCaptureDescr = directSupertypeSig.getNonSubstitutedCaptureDescr,
                allowedPackages = builtCtx.packages.keySet,
                allowedDevices = Device.values.toSet
              )
              for ((fldName, superFldInfo) <- directSupertypeSig.fields) {
                structSig.fields.get(fldName) match {
                  case Some(subFieldInfo) =>
                    if (subFieldInfo.isReassignable != superFldInfo.isReassignable) {
                      reportError(s"$fldName should be reassignable in $structId if and only if it is " +
                        s"reassignable in its supertype $directSupertypeId", posOpt)
                    } else if (subFieldInfo.isReassignable && subFieldInfo.tpe != superFldInfo.tpe) {
                      reportError(s"reassignable field $fldName should have the same type in $structId as in its " +
                        s"supertype $directSupertypeId", posOpt)
                    } else if (
                      !subFieldInfo.isReassignable
                        && !subFieldInfo.tpe.subtypeOf(superFldInfo.tpe)(using tcCtx, OcapEnabled)
                    ) {
                      reportError(s"type ${subFieldInfo.tpe} of field $fldName does not conform to its type " +
                        s"${superFldInfo.tpe} in the interface $directSupertypeId", posOpt)
                    }
                  case None =>
                    reportError(s"$structId cannot subtype $directSupertypeId: missing field $fldName", posOpt)
                }
                tcCtx.addLocal(fldName, superFldInfo.tpe, None, superFldInfo.isReassignable, true, () => (), () => ())
              }
            } else {
              reportError(s"struct $directSupertypeId is not an interface", posOpt)
            }
          }
          case None =>
            reportError(s"interface $directSupertypeId is unknown", posOpt)
        }
      }
    }

    private def checkSubtypingCycles(builtStructMap: Map[TypeIdentifier, StructSignature]): Unit = {

      // DFS

      val maxDepth = builtStructMap.size
      var found = false
      val iter = structs.iterator
      while (iter.hasNext && !found) {

        val (currId, (_, posOpt)) = iter.next()

        def findCycleFrom(start: TypeIdentifier, target: TypeIdentifier, depth: Int): Unit = {
          if (start == target && depth > 0) {
            reportError(s"cyclic subtyping: cycle involves $start", posOpt)
            found = true
          } else if (depth < maxDepth) {
            for (child <- builtStructMap.apply(start).directSupertypes) {
              findCycleFrom(child, target, depth + 1)
            }
          }
        }

        findCycleFrom(currId, currId, depth = 0)
      }
    }

  }

}
