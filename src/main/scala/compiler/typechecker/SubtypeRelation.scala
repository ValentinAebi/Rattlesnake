package compiler.typechecker

import compiler.typechecker.TypeCheckingContext
import compiler.typechecker.SubcaptureRelation.subcaptureOf
import identifiers.TypeIdentifier
import lang.StructSignature
import lang.Types.*
import lang.Types.PrimitiveTypeShape.*

import scala.collection.mutable

object SubtypeRelation {

  extension (l: Type) def subtypeOf(r: Type)(using TypeCheckingContext): Boolean =
    l.shape.subtypeOf(r.shape) && l.captureDescriptor.subcaptureOf(r.captureDescriptor)

  extension (subT: TypeShape) def subtypeOf(superT: TypeShape)(using TypeCheckingContext): Boolean = {
    (subT, superT) match {
      case _ if subT == superT => true
      case (NothingType | UndefinedTypeShape, _) => true
      case (NamedTypeShape(subId), NamedTypeShape(superId)) =>
        subInterfaceOf(subId, superId)
      case (ArrayTypeShape(subElemType, subIsModif), ArrayTypeShape(superElemType, superIsModif)) =>
        logicalImplies(superIsModif, subIsModif) &&
          (subElemType == superElemType || (!superIsModif && subElemType.subtypeOf(superElemType)))
      case (UnionTypeShape(subTypes), superT) =>
        subTypes.forall(_.subtypeOf(superT))
      case (subT, UnionTypeShape(superTypes)) =>
        superTypes.exists(subT.subtypeOf(_))
      case _ => false
    }
  }

  def subInterfaceOf(subT: TypeIdentifier, superT: TypeIdentifier)(using tcCtx: TypeCheckingContext): Boolean = {
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
      tcCtx.structs.get(curr).foreach(_.directSupertypes.foreach(registerIfNew))
    }
    false
  }

  private def logicalImplies(a: Boolean, b: Boolean): Boolean = (!a || b)

}
