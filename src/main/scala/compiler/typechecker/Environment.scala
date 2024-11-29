package compiler.typechecker

import identifiers.TypeIdentifier
import lang.Device
import lang.Types.NamedTypeShape

final case class Environment(allowedPackages: Set[TypeIdentifier], allowedDevices: Set[Device]) {

  def allowsPackage(pkg: TypeIdentifier): Boolean =
    allowedPackages.contains(pkg)

  def allowsDevice(device: Device): Boolean =
    allowedDevices.contains(device)

}

object Environment {

  val empty: Environment = Environment(
    Set.empty,
    Set.empty
  )

}
