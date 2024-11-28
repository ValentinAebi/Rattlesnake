package compiler.typechecker

import identifiers.TypeIdentifier
import lang.Device
import lang.Types.NamedTypeShape

final case class Environment(
                              currentModuleType: NamedTypeShape,
                              allowedPackages: Set[TypeIdentifier],
                              allowedDevices: Set[Device]
                            ) {

  def allowsPackage(pkg: TypeIdentifier): Boolean =
    allowedPackages.contains(pkg)

  def allowsDevice(device: Device): Boolean =
    allowedDevices.contains(device)

}

object Environment {
  
  def emptyFrom(currentModuleType: NamedTypeShape): Environment = Environment(
    currentModuleType,
    Set.empty,
    Set.empty
  )
  
}
