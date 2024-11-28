package compiler.typechecker

import compiler.analysisctx.AnalysisContext
import compiler.irs.Asts
import compiler.irs.Asts.*
import compiler.pipeline.CompilationStep.TypeChecking
import compiler.reporting.Errors.{Err, ErrorReporter, errorsExitCode}
import compiler.reporting.Position
import lang.*
import lang.Types.NamedTypeShape

object PathsConverter {

  def convertOrFailSilently(expr: Expr): Option[Capturable] = expr match {
    case VariableRef(name) => Some(IdPath(name))
    case MeRef() => Some(MePath)
    case PackageRef(pkgName) => Some(CapPackage(pkgName))
    case DeviceRef(device) => Some(CapDevice(device))
    case Select(lhs, selected) =>
      convertOrFailSilently(lhs).flatMap {
        case p: Path => Some(SelectPath(p, selected))
        case _ => None
      }
    case _ => None
  }

  def convertAndCheck(expr: Expr, tcCtx: TypeCheckingContext, er: ErrorReporter): Option[ConcreteCapturable] = {
    
    def reportError(msg: String, posOpt: Option[Position]): None.type = {
      er.push(Err(TypeChecking, msg, posOpt))
      None
    }
    
    expr match {
      case VariableRef(name) =>
        Some(IdPath(name))
      case MeRef() => Some(MePath)
      case PackageRef(pkgName) => Some(CapPackage(pkgName))
      case DeviceRef(device) => Some(CapDevice(device))
      case Select(lhs, selected) =>
        convertAndCheck(lhs, tcCtx, er).flatMap {
          case lhsPath: Path => {
            lhs.getType match {
              case NamedTypeShape(lhsTypeId) =>
                tcCtx.resolveType(lhsTypeId).flatMap { lhsTypeSig =>
                  lhsTypeSig.params.get(selected).flatMap { fieldInfo =>
                    if fieldInfo.isReassignable
                    then reportError(s"cannot capture field $selected of $lhsTypeId, as it is reassignable", expr.getPosition)
                    else Some(lhsPath.dot(selected))
                  }
                }
              case lhsType => None
            }
          }
          case _ => None
        }
      case _ => reportError(s"not a path", expr.getPosition)
    }
  }

}
