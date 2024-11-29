package lang

object CaptureDescriptors {

  sealed trait CaptureDescriptor {

    def isRoot: Boolean

    def isEmpty: Boolean

    def union(that: CaptureDescriptor): CaptureDescriptor = (this, that) match {
      case (CaptureSet(thisCs), CaptureSet(thatCs)) => CaptureSet(thisCs.union(thatCs))
      case _ => Brand
    }

  }

  case object Brand extends CaptureDescriptor {
    override def isRoot: Boolean = false

    override def isEmpty: Boolean = false

    override def toString: String = "?"
  }

  final case class CaptureSet(set: Set[Capturable]) extends CaptureDescriptor {
    override def isRoot: Boolean = set.contains(RootCapability)

    override def isEmpty: Boolean = set.isEmpty

    override def toString: String = set.mkString("{", ",", "}")
  }

  object CaptureSet {

    def apply(values: Capturable*): CaptureSet = CaptureSet(Set(values *))

    val empty: CaptureSet = CaptureSet(Set.empty)
    val singletonOfRoot: CaptureSet = CaptureSet(RootCapability)

  }

  def unionOf(captureDescriptors: Iterable[CaptureDescriptor]): CaptureDescriptor = {
    captureDescriptors.foldLeft[CaptureDescriptor](CaptureSet.empty)(_.union(_))
  }

}
