package compiler.ircornegen

import compiler.identifiers.TypeIdentifier

import scala.collection.mutable


final class ClosuresNamer {
  private val closureNames = mutable.ListBuffer.empty[TypeIdentifier]
  private var nextUid = 0L

  def mkName(packagePrefixes: List[String], closureOwnerDescr: String): TypeIdentifier = {
    val uid = nextUid
    nextUid += 1
    val closureName = TypeIdentifier(packagePrefixes, "Closure$" + uid + "$" + closureOwnerDescr)
    closureNames.addOne(closureName)
    closureName
  }
  
  def closures: Iterable[TypeIdentifier] = closureNames.toList

}
