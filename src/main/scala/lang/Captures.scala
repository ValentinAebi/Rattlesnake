package lang

import identifiers.{FunOrVarId, TypeIdentifier}

object Captures {

  sealed trait CaptureDescriptor {
    def isCapSetOfPureType: Boolean

    def prettified: String

    def toHatNotation: String

    infix def ++(that: CaptureDescriptor): CaptureDescriptor = (this, that) match {
      case (CaptureSet(l), CaptureSet(r)) => CaptureSet(l ++ r)
      case _ => Brand
    }

    def removed(rem: Iterable[Capturable]): CaptureDescriptor
    
    def union(that: CaptureDescriptor): CaptureDescriptor = (this, that) match {
      case (CaptureSet(l), CaptureSet(r)) => CaptureSet(l ++ r)
      case _ => Brand
    }

  }

  case class CaptureSet(captured: Set[Capturable]) extends CaptureDescriptor {
    override def isCapSetOfPureType: Boolean = captured.isEmpty

    override def removed(rem: Iterable[Capturable]): CaptureSet = CaptureSet(captured -- rem)

    override def prettified: String = {
      if captured.isEmpty then ""
      else captured.mkString("{", ",", "}")
    }

    override def toHatNotation: String = {
      if captured.isEmpty then ""
      else captured.mkString("^{", ",", "}")
    }

    override def toString: String = captured.mkString("{", ",", "}")
  }

  object CaptureSet {
    def apply(capt: Capturable*): CaptureSet = CaptureSet(capt.toSet)

    def empty: CaptureSet = CaptureSet(Set.empty)
    
    def singletonOfRoot: CaptureSet = CaptureSet(Set(RootCapability))
  }

  case object Brand extends CaptureDescriptor {
    override def isCapSetOfPureType: Boolean = false

    override def removed(rem: Iterable[Capturable]): Brand.type = this

    override def prettified: String = "#"

    override def toHatNotation: String = "^#"

    override def toString: String = "#"
  }

  sealed trait Capturable {
    def isRootedIn(varId: FunOrVarId): Boolean
  }

  sealed trait StablePath

  sealed trait ProperPath extends StablePath, Capturable

  case class VarPath(root: FunOrVarId) extends ProperPath {
    override def isRootedIn(varId: FunOrVarId): Boolean = (varId == root)

    override def toString: String = root.toString
  }
  
  case object MePath extends ProperPath {
    override def isRootedIn(varId: FunOrVarId): Boolean = false

    override def toString: String = Keyword.Me.str
  }
  
  case class PackagePath(packageName: TypeIdentifier) extends ProperPath {
    override def isRootedIn(varId: FunOrVarId): Boolean = false
  }
  
  case class DevicePath(device: Device) extends ProperPath {
    override def isRootedIn(varId: FunOrVarId): Boolean = false
  }

  case class SelectPath(lhs: ProperPath, field: FunOrVarId) extends ProperPath {
    override def isRootedIn(varId: FunOrVarId): Boolean = lhs.isRootedIn(varId)

    override def toString: String = s"$lhs.$field"
  }

  case class BrandedPath(p: ProperPath) extends StablePath {
    export p.isRootedIn

    override def toString: String = s"#$p"
  }

  case object RootCapability extends Capturable {
    override def isRootedIn(varId: FunOrVarId): Boolean = false

    override def toString: String = "cap"
  }
  
}
