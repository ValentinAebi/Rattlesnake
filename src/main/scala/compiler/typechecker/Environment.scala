package compiler.typechecker

import identifiers.TypeIdentifier
import lang.Device

final case class Environment(
                              currentModule: TypeIdentifier,
                              allowedPackages: Set[TypeIdentifier],
                              allowedDevices: Set[Device]
                            ) {

  def allowsPackage(pkg: TypeIdentifier): Boolean =
    allowedPackages.contains(pkg)

  def allowsDevice(device: Device): Boolean =
    allowedDevices.contains(device)

}
