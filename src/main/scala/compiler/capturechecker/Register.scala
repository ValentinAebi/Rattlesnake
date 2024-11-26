package compiler.capturechecker

import identifiers.{FunOrVarId, TypeIdentifier}
import lang.Device

sealed trait Register
final class GenRegister extends Register
final case class VarRegister(id: FunOrVarId) extends Register
case object MeRegister extends Register
final case class PackageRegister(pkgId: TypeIdentifier) extends Register
final case class DeviceRegister(device: Device) extends Register
