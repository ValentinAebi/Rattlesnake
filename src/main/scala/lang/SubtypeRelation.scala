package lang

import compiler.typechecker.TypeCheckingContext
import identifiers.TypeIdentifier
import lang.SubcaptureRelation.{SubcapturingContext, subcaptureOf}
import lang.Types.*
import lang.Types.PrimitiveType.*

import scala.collection.mutable

object SubtypeRelation {

  extension (subT: Type) def subtypeOf(superT: Type)(using ctx: SubcapturingContext): Boolean = {
    (subT, superT) match {
      case _ if subT == superT => true
      case (NothingType | UndefinedType, _) => true
      case (NamedType(subId, subcs), NamedType(superId, supercs)) =>
        subInterfaceOf(subId, superId, ctx) && subcs.subcaptureOf(supercs)
      case (ArrayType(subElemType, subIsModif), ArrayType(superElemType, superIsModif)) =>
        logicalImplies(superIsModif, subIsModif) &&
          (subElemType == superElemType || (!superIsModif && superElemType.subtypeOf(superElemType)))
      case (UnionType(subTypes), superT) =>
        subTypes.forall(_.subtypeOf(superT))
      case (subT, UnionType(superTypes)) =>
        superTypes.exists(subT.subtypeOf(_))
      case _ => false
    }
  }

  def subInterfaceOf(subT: TypeIdentifier, superT: TypeIdentifier, ctx: SubcapturingContext): Boolean = {
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
      ctx.structs.apply(curr).directSupertypes.foreach(registerIfNew)
    }
    false
  }

  private def logicalImplies(a: Boolean, b: Boolean): Boolean = (!a || b)

}
