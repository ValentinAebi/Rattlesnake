package compiler.typechecker

import compiler.irs.Asts.*
import lang.Capturables.*

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
  
}
