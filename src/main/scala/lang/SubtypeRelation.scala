package lang

import identifiers.TypeIdentifier
import lang.StructSignature
import lang.Types.*
import lang.Types.PrimitiveType.*

import scala.collection.mutable

object SubtypeRelation {

  extension (subT: Type) def subtypeOf(superT: Type)(using Map[TypeIdentifier, StructSignature]): Boolean = {
    (subT, superT) match {
      case _ if subT == superT => true
      case (NothingType | UndefinedType, _) => true
      case (StructType(subId, subIsModif), StructType(superId, superIsModif)) =>
        logicalImplies(superIsModif, subIsModif) && isSuperInterface(subId, superId)
      case (ArrayType(subElemType, subIsModif), ArrayType(superElemType, superIsModif)) =>
        logicalImplies(superIsModif, subIsModif) &&
          (subElemType == superElemType || (!superIsModif && subElemType.subtypeOf(superElemType)))
      case (UnionType(subTypes), superT) =>
        subTypes.forall(_.subtypeOf(superT))
      case (subT, UnionType(superTypes)) =>
        superTypes.exists(subT.subtypeOf(_))
      case _ => false
    }
  }

  private def isSuperInterface(subT: TypeIdentifier, superT: TypeIdentifier)
                              (using structs: Map[TypeIdentifier, StructSignature]): Boolean = {
    // BFS

    val worklist = mutable.Queue.empty[TypeIdentifier]
    val alreadyAdded = mutable.Set.empty[TypeIdentifier]

    def registerIfNew(tid: TypeIdentifier): Unit = {
      if (!alreadyAdded.contains(tid)) {
        worklist.enqueue(tid)
        alreadyAdded.addOne(tid)
      }
    }

    registerIfNew(subT)
    while (worklist.nonEmpty) {
      val curr = worklist.dequeue()
      if (curr == superT) {
        return true
      }
      structs.apply(curr).directSupertypes.foreach(registerIfNew)
    }
    false
  }

  private def logicalImplies(a: Boolean, b: Boolean): Boolean = (!a || b)

}
