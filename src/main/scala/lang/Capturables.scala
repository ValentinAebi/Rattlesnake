package lang

import identifiers.{FunOrVarId, TypeIdentifier}
import lang.Device
import lang.Types.Type

object Capturables {

  sealed trait Capturable

  sealed trait ConcreteCapturable extends Capturable

  sealed trait Path extends ConcreteCapturable {
    def root: RootPath
    def dot(fld: FunOrVarId): Path = SelectPath(this, fld)
  }
  
  sealed trait RootPath extends Path {
    override def root: RootPath = this
  }

  final case class IdPath(id: FunOrVarId) extends RootPath {
    override def toString: String = id.stringId
  }

  case object MePath extends RootPath {
    override def toString: String = Keyword.Me.str
  }

  final case class SelectPath(directRoot: Path, fld: FunOrVarId) extends Path {
    override def root: RootPath = directRoot.root
    override def toString: String = s"$directRoot.$fld"
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
