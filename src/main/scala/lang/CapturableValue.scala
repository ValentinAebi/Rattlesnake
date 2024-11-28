package lang

import identifiers.{FunOrVarId, TypeIdentifier}
import lang.Device
import lang.Types.Type

sealed trait CapturableValue {
  def dot(fld: FunOrVarId): CapturableValue = SelectValue(this, fld)
}

final case class PackageValue(typeIdentifier: TypeIdentifier) extends CapturableValue {
  override def toString: String = typeIdentifier.stringId
}

final case class DeviceValue(device: Device) extends CapturableValue {
  override def toString: String = device.keyword.str
}

final case class FunctionParamValue(paramId: FunOrVarId) extends CapturableValue {
  override def toString: String = paramId.stringId
}

final case class LocalVarValue(varId: FunOrVarId) extends CapturableValue {
  override def toString: String = varId.stringId
}

final case class MeValue() extends CapturableValue {
  override def toString: String = "me"
}

final case class SelectValue(root: CapturableValue, field: FunOrVarId) extends CapturableValue {
  override def toString: String = s"$root.$field"
}

final class AnonymousValue extends CapturableValue {
  override def toString: String = "<anonymous>"
}

case object RootCapValue extends CapturableValue {
  override def toString: String = "<cap>"
}

final class UndefinedValue extends CapturableValue {
  override def tpe: Type = Types.UndefinedTypeShape
  override def toString: String = "<undefined>"
}
