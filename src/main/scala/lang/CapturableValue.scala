package lang

import identifiers.TypeIdentifier
import lang.Device

sealed trait CapturableValue

final case class PackageValue(typeIdentifier: TypeIdentifier) extends CapturableValue {
  override def toString: String = typeIdentifier.stringId
}

final case class DeviceValue(device: Device) extends CapturableValue {
  override def toString: String = device.keyword.str
}

final class ProgramValue(descr: String) extends CapturableValue {
  override def toString: String = descr
}

case object RootCapValue extends CapturableValue {
  override def toString: String = "<cap>"
}
