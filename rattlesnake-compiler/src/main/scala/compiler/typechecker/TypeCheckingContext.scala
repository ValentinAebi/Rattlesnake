package compiler.typechecker

import compiler.analysisctx.AnalysisContext
import compiler.pipeline.CompilationStep.TypeChecking
import compiler.reporting.Errors.{ErrorReporter, Warning}
import compiler.reporting.{Errors, Position}
import compiler.typechecker.TypeCheckingContext.{LocalInfo, LocalUsesCollector}
import identifiers.{FunOrVarId, TypeIdentifier}
import lang.*
import lang.Capturables.{ConcreteCapturable, IdPath, Path}
import lang.CaptureDescriptors.*
import lang.Types.*
import lang.Types.PrimitiveTypeShape.{NothingType, VoidType}

import scala.collection.mutable

/**
 * Mutable context for type checking
 */
final case class TypeCheckingContext private(
                                              analysisContext: AnalysisContext,
                                              environment: Environment,
                                              private val locals: mutable.Map[FunOrVarId, LocalInfo] = mutable.Map.empty,
                                              meTypeId: TypeIdentifier,
                                              meCaptureDescr: CaptureDescriptor,
                                              allowedPackages: Set[TypeIdentifier],
                                              allowedDevices: Set[Device]
                                            ) {

  def meType: Type = NamedTypeShape(meTypeId) ^ meCaptureDescr

  // Locals that have been created by this context (i.e. not obtained via a copy)
  private val ownedLocals: mutable.Set[FunOrVarId] = mutable.Set.empty

  /**
   * @return a copy of this with empty locals map
   */
  def copyForNewFunction: TypeCheckingContext =
    copy(locals = mutable.Map.empty)

  /**
   * @return a (deep) copy of this
   */
  def copied: TypeCheckingContext =
    copy(locals = mutable.Map.from(locals))

  // TODO keep smartcasts on vars, until reassignment, and re-enable them on loops
  def copyWithSmartCasts(smartCasts: Map[FunOrVarId, TypeShape]): TypeCheckingContext = {
    copy(locals = locals.map {
      case (id, info) => id -> info.copy(tpe = smartCasts.getOrElse(id, info.tpe))
    })
  }

  def copyWithEnvironment(envirMaker: Environment => Environment): TypeCheckingContext =
    copy(environment = envirMaker(environment))

  /**
   * Register a new local
   *
   * @param name                  name of the local
   * @param tpe                   type of the local
   * @param isReassignable        whether it is allowed or not to assign a new value to this local (`val` vs `var`)
   * @param duplicateVarCallback  to be called if the name is already used by another local
   * @param forbiddenTypeCallback to be called if the local has a type that is not acceptable for a local
   */
  def addLocal(
                name: FunOrVarId,
                tpe: Type,
                defPos: Option[Position],
                isReassignable: Boolean,
                declHasTypeAnnot: Boolean,
                duplicateVarCallback: () => Unit,
                forbiddenTypeCallback: () => Unit
              ): Unit = {
    if (tpe == NothingType || tpe == VoidType) {
      forbiddenTypeCallback()
    } else if (locals.contains(name)) {
      duplicateVarCallback()
    } else {
      locals.put(name, LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, environment))
      ownedLocals.addOne(name)
    }
  }

  def getLocalOnly(name: FunOrVarId): Option[LocalInfo] = locals.get(name)

  def getLocalOrConst(name: FunOrVarId): Option[LocalInfo] = {
    getLocalOnly(name).orElse(
      analysisContext.constants
        .get(name)
        // defPos and declHasTypeAnnot are never used for constants, as long as constants can only be of primitive types
        .map(tpe => LocalInfo(name, tpe, isReassignable = false, defPos = None, declHasTypeAnnot = false, RootEnvir))
    )
  }

  def lookup(path: Path): Type = path match {
    case Capturables.IdPath(id) =>
      getLocalOnly(id).filter(!_.isReassignable).map(_.tpe)
        .getOrElse(UndefinedTypeShape)
    case Capturables.MePath => meType
    case Capturables.SelectPath(directRoot, fld) =>
      lookup(directRoot).shape match {
        case NamedTypeShape(typeName) =>
          resolveTypeAs[SelectableSig](typeName).flatMap(_.typeOfSelectIfCapturable(fld))
            .getOrElse(UndefinedTypeShape)
        case _ => UndefinedTypeShape
      }
  }

  def lookup(capturable: ConcreteCapturable): Type = capturable match {
    case path: Path => lookup(path)
    case Capturables.CapPackage(pkgName) =>
      packages.get(pkgName).map(_.getNonSubstitutedType)
        .getOrElse(UndefinedTypeShape)
    case Capturables.CapDevice(device) =>
      device.tpe
  }

  def localIsQueried(localId: FunOrVarId): Unit = {
    locals.get(localId).foreach { l =>
      l.usesCollector.queried = true
    }
  }

  def varIsAssigned(varId: FunOrVarId): Unit = {
    locals.get(varId).foreach { l =>
      l.usesCollector.reassigned = true
    }
  }

  def writeLocalsRelatedWarnings(errorReporter: ErrorReporter): Unit = {
    for (_, LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, declaringEnvir, usesCollector)) <- locals if ownedLocals.contains(name) do {
      if (!usesCollector.queried) {
        errorReporter.push(Warning(TypeChecking, s"unused local: '$name' is never queried", defPos))
      } else if (isReassignable && !usesCollector.reassigned) {
        errorReporter.push(Warning(TypeChecking, s"value declared as variable: '$name' could be a ${Keyword.Val}", defPos))
      }
    }
  }

  export analysisContext.*

}

object TypeCheckingContext {

  def apply(
             analysisContext: AnalysisContext,
             environment: Environment,
             meTypeId: TypeIdentifier,
             meCaptureDescr: CaptureDescriptor,
             allowedPackages: Set[TypeIdentifier],
             allowedDevices: Set[Device]
           ): TypeCheckingContext = {
    TypeCheckingContext(analysisContext, environment, mutable.Map.empty, meTypeId,
      meCaptureDescr, allowedPackages, allowedDevices)
  }

  final case class LocalInfo private(
                                      name: FunOrVarId,
                                      tpe: Type,
                                      isReassignable: Boolean,
                                      defPos: Option[Position],
                                      declHasTypeAnnot: Boolean,
                                      declaringEnvir: Environment,
                                      usesCollector: LocalUsesCollector
                                    ) {
    def copy(
              name: FunOrVarId = name,
              tpe: Type = tpe,
              isReassignable: Boolean = isReassignable,
              defPos: Option[Position] = defPos,
              declHasTypeAnnot: Boolean = declHasTypeAnnot,
              declaringEnvir: Environment = declaringEnvir,
              usesCollector: LocalUsesCollector = usesCollector
            ): LocalInfo = {
      LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, declaringEnvir, usesCollector)
    }
  }

  object LocalInfo {
    def apply(
               name: FunOrVarId,
               tpe: Type,
               isReassignable: Boolean,
               defPos: Option[Position],
               declHasTypeAnnot: Boolean,
               declaringEnvir: Environment,
             ): LocalInfo = {
      LocalInfo(name, tpe, isReassignable, defPos, declHasTypeAnnot, declaringEnvir, new LocalUsesCollector())
    }
  }

  final class LocalUsesCollector {
    var queried = false
    var reassigned = false
  }

}
