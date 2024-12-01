package lang

import identifiers.{FunOrVarId, TypeIdentifier}
import lang.Device
import lang.Types.Type

object Capturables {

  sealed trait Capturable

  sealed trait ConcreteCapturable extends Capturable

  sealed trait Path extends ConcreteCapturable {
    def dot(fld: FunOrVarId): Path = SelectPath(this, fld)
  }

  final case class IdPath(id: FunOrVarId) extends Path {
    override def toString: String = id.stringId
  }

  final case class SelectPath(root: Path, fld: FunOrVarId) extends Path {
    override def toString: String = s"$root.$fld"
  }

  case object MePath extends Path {
    override def toString: String = Keyword.Me.str
  }

  final case class CapPackage(pkgName: TypeIdentifier) extends ConcreteCapturable {
    override def toString: String = pkgName.stringId
  }

  final case class CapDevice(device: Device) extends ConcreteCapturable {
    override def toString: String = device.toString
  }

  case object RootCapability extends Capturable {
    override def toString: String = Keyword.Cap.str
  }
  
}
