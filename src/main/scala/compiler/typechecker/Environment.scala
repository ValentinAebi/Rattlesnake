package compiler.typechecker

import identifiers.TypeIdentifier
import lang.Device
import lang.Types.NamedType

final case class Environment(
                              currentModuleType: NamedType,
                              allowedPackages: Set[TypeIdentifier],
                              allowedDevices: Set[Device]
                            ) {

  def allowsPackage(pkg: TypeIdentifier): Boolean =
    allowedPackages.contains(pkg)

  def allowsDevice(device: Device): Boolean =
    allowedDevices.contains(device)

}
