package lang

sealed trait CaptureDescriptor {
  
  def isRoot: Boolean
  def isEmpty: Boolean
  
  def union(that: CaptureDescriptor): CaptureDescriptor = (this, that) match {
    case (CaptureSet(thisCs), CaptureSet(thatCs)) => CaptureSet(thisCs.union(thatCs))
    case _ => Brand
  }
  
}

final case class CaptureSet(set: Set[Capturable]) extends CaptureDescriptor {
  override def isRoot: Boolean = set.contains(RootCapability)
  override def isEmpty: Boolean = set.isEmpty

  override def toString: String = set.mkString("{", ",", "}")
}

case object Brand extends CaptureDescriptor {
  override def isRoot: Boolean = false
  override def isEmpty: Boolean = false

  override def toString: String = "?"
}

object CaptureSet {

  def apply(values: Capturable*): CaptureSet = CaptureSet(Set(values *))

  val empty: CaptureSet = CaptureSet(Set.empty)
  val singletonOfRoot: CaptureSet = CaptureSet(RootCapability)

}
