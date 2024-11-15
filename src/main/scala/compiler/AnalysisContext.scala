package compiler

import compiler.AnalysisContext.{FunctionFound, FunctionNotFound, MethodResolutionResult, ModuleNotFound}
import compiler.CompilationStep.ContextCreation
import compiler.Errors.{Err, ErrorReporter, errorsExitCode}
import compiler.irs.Asts.*
import identifiers.{FunOrVarId, IntrinsicsPackageId, TypeIdentifier}
import lang.*
import lang.SubcaptureRelation.SubcapturingContext
import lang.SubtypeRelation.subtypeOf
import lang.Types.PrimitiveType.{NothingType, VoidType}
import lang.Types.Type

import scala.collection.mutable

final case class AnalysisContext(
                                  modules: Map[TypeIdentifier, ModuleSignature],
                                  packages: Map[TypeIdentifier, PackageSignature],
                                  structs: Map[TypeIdentifier, StructSignature],
                                  constants: Map[FunOrVarId, Type]
                                ) {

  /**
   * Returns `true` iff `tpe` is known (primitive type, known struct or array of a known type)
   */
  // do not use @tailrec, even though Intellij suggests it: fails the CI
  def knowsType(tpe: Type): Boolean = {
    tpe match {
      case _: Types.PrimitiveType => true
      case Types.NamedType(typeName, captureDescr) => knowsUserDefType(typeName)
      case Types.ArrayType(elemType, _) => knowsType(elemType)
      case Types.UnionType(unitedTypes) => unitedTypes.forall(knowsType)
      case Types.UndefinedType => true
    }
  }

  def knowsUserDefType(tid: TypeIdentifier): Boolean =
    knowsPackageOrModule(tid) || knowsStruct(tid)

  def knowsPackageOrModule(tid: TypeIdentifier): Boolean = knowsPackage(tid) || knowsModule(tid)

  def knowsPackage(tid: TypeIdentifier): Boolean = packages.contains(tid)

  def knowsModule(tid: TypeIdentifier): Boolean = modules.contains(tid)

  def knowsStruct(tid: TypeIdentifier): Boolean = structs.contains(tid)

  def resolveType(tid: TypeIdentifier): Option[TypeSignature] =
    structs.get(tid).orElse(modules.get(tid)).orElse(packages.get(tid))

  def resolveFunc(owner: TypeIdentifier, methodName: FunOrVarId): MethodResolutionResult = {
    modules.get(owner)
      .orElse(packages.get(owner))
      .orElse(Device.deviceTypeToSig.get(owner))
      .map { modOrPkg =>
        modOrPkg.functions.get(methodName) map { sig =>
          FunctionFound(sig)
        } getOrElse FunctionNotFound
      } getOrElse ModuleNotFound
  }

}

object AnalysisContext {

  sealed trait MethodResolutionResult {
    def getOrThrow(): FunctionSignature
  }

  final case class FunctionFound(signature: FunctionSignature) extends MethodResolutionResult {
    override def getOrThrow(): FunctionSignature = signature
  }

  object ModuleNotFound extends MethodResolutionResult {
    override def getOrThrow(): FunctionSignature = throw new NoSuchElementException()
  }

  object FunctionNotFound extends MethodResolutionResult {
    override def getOrThrow(): FunctionSignature = throw new NoSuchElementException()
  }

  final class Builder(errorReporter: ErrorReporter) {
    private val modules: mutable.Map[TypeIdentifier, ModuleSignature] = mutable.Map.empty
    private val packages: mutable.Map[TypeIdentifier, PackageSignature] = mutable.Map(
      IntrinsicsPackageId -> PackageSignature(
        name = IntrinsicsPackageId,
        importedPackages = mutable.LinkedHashSet.empty,
        importedDevices = mutable.LinkedHashSet.empty,
        functions = Intrinsics.intrinsics
      )
    )
    private val structs: mutable.Map[TypeIdentifier, (StructSignature, Option[Position])] = mutable.Map.empty
    private val constants: mutable.Map[FunOrVarId, Type] = mutable.Map.empty

    def addModule(moduleDef: ModuleDef): Unit = {
      val moduleName = moduleDef.moduleName
      if (checkTypeNotAlreadyDefined(moduleName, moduleDef.getPosition)) {
        val (importedModules, importedPackages, importedDevices) = analyzeImports(moduleDef)
        val functions = extractFunctions(moduleDef)
        val moduleSig = ModuleSignature(moduleName, importedModules, importedPackages, importedDevices, functions)
        modules.put(moduleName, moduleSig)
      }
    }

    def addPackage(packageDef: PackageDef): Unit = {
      val packageName = packageDef.packageName
      if (checkTypeNotAlreadyDefined(packageName, packageDef.getPosition)) {
        val functions = extractFunctions(packageDef)
        val (implicitlyImportedPackages, implicitlyImportedDevices) = trackPackagesAndDevices(packageDef)
        val sig = PackageSignature(packageName, implicitlyImportedPackages, implicitlyImportedDevices, functions)
        packages.put(packageName, sig)
      }
    }

    def addStruct(structDef: StructDef): Unit = {
      val name = structDef.structName
      if (checkTypeNotAlreadyDefined(name, structDef.getPosition)) {
        val fieldsMap = buildFieldsMap(structDef)
        val sig = StructSignature(name, fieldsMap, structDef.directSupertypes, structDef.isInterface)
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
      val builtStructsMap = structs.map((tid, sigAndPos) => (tid, sigAndPos._1)).toMap
      checkSubtyping(builtStructsMap)
      checkSubtypingCycles(builtStructsMap)
      new AnalysisContext(
        modules.toMap,
        packages.toMap,
        builtStructsMap,
        constants.toMap
      )
    }

    private def extractFunctions(repo: ModuleOrPackageDefTree): Map[FunOrVarId, FunctionSignature] = {
      val functions = mutable.Map.empty[FunOrVarId, FunctionSignature]
      for funDef <- repo.functions do {
        val name = funDef.funName
        if (functions.contains(name)) {
          reportError(s"redefinition of function '$name'", funDef.getPosition)
        } else {
          functions.put(name, funDef.signature)
        }
      }
      functions.toMap
    }

    private def analyzeImports(moduleDef: ModuleDef) = {
      val importsMap = new mutable.LinkedHashMap[FunOrVarId, TypeIdentifier]()
      val packagesSet = new mutable.LinkedHashSet[TypeIdentifier]()
      val devicesSet = new mutable.LinkedHashSet[Device]()
      moduleDef.imports.foreach {
        case ModuleImport(instanceId, moduleId) =>
          importsMap.put(instanceId, moduleId)
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

    private def buildFieldsMap(structDef: StructDef) = {
      val fieldsMap = new mutable.LinkedHashMap[FunOrVarId, StructSignature.FieldInfo]()
      for param <- structDef.fields do {
        param.paramNameOpt match {
          case None =>
            reportError("struct fields must be named", param.getPosition)
          case Some(paramName) =>
            if (checkIsNotVoidOrNothing(param.tpe, param.getPosition)) {
              if (fieldsMap.contains(paramName)) {
                reportError(s"duplicate field: '$paramName'", param.getPosition)
              } else {
                fieldsMap.put(paramName, StructSignature.FieldInfo(param.tpe, param.isReassignable))
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

    private def checkSubtyping(builtStructMap: Map[TypeIdentifier, StructSignature]): Unit = {
      for {
        (structId, (structSig, posOpt)) <- structs
        directSupertypeId <- structSig.directSupertypes
      } {
        structs.get(directSupertypeId) match {
          case Some((directSupertypeSig, _)) => {
            if (directSupertypeSig.isInterface) {
              val previousFields = mutable.Map.empty[FunOrVarId, Type]
              for ((fldName, superFldInfo) <- directSupertypeSig.fields) {
                structSig.fields.get(fldName) match {
                  case Some(subFieldInfo) =>
                    if (logicalImplies(superFldInfo.isReassignable, subFieldInfo.isReassignable)) {
                      checkFieldVariance(structId, directSupertypeId, fldName, previousFields.toMap,
                        subFieldInfo, superFldInfo, builtStructMap, posOpt)
                    } else {
                      errorReporter.push(Err(ContextCreation,
                        s"subtyping error: $fldName needs to be reassignable in subtypes of $directSupertypeId", posOpt))
                    }
                  case None =>
                    errorReporter.push(Err(ContextCreation,
                      s"$structId cannot subtype $directSupertypeId: missing field $fldName", posOpt))
                }
                previousFields.addOne(fldName -> superFldInfo.tpe)
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

    private def checkFieldVariance(
                                    structId: TypeIdentifier,
                                    directSupertypeId: TypeIdentifier,
                                    fldName: FunOrVarId,
                                    previousFields: Map[FunOrVarId, Type],
                                    subFieldInfo: StructSignature.FieldInfo, superFldInfo: StructSignature.FieldInfo,
                                    structs: Map[TypeIdentifier, StructSignature],
                                    posOpt: Option[Position]
                                  ): Unit = {
      if (superFldInfo.isReassignable && subFieldInfo.tpe != superFldInfo.tpe) {
        errorReporter.push(Err(ContextCreation,
          s"subtyping error: type of $fldName is not the same in $structId and $directSupertypeId", posOpt))
      } else if (
        !superFldInfo.isReassignable
          && !subFieldInfo.tpe.subtypeOf(superFldInfo.tpe)(using SubcapturingContext(previousFields, structs))) {
        errorReporter.push(Err(ContextCreation,
          s"subtyping error: type of $fldName in $structId should be a subtype of its type in $directSupertypeId", posOpt))
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

    private def logicalImplies(p: Boolean, q: Boolean): Boolean = !p || q

  }

}
