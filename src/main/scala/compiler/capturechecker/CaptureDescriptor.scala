package compiler.capturechecker

import lang.{CapturableValue, RootCapValue}

sealed trait CaptureDescriptor {
  
  def union(that: CaptureDescriptor): CaptureDescriptor = (this, that) match {
    case (CaptureSet(thisCs), CaptureSet(thatCs)) => CaptureSet(thisCs.union(thatCs))
    case _ => Brand
  }
  
}

final case class CaptureSet(set: Set[CapturableValue]) extends CaptureDescriptor

case object Brand extends CaptureDescriptor

object CaptureSet {

  def apply(values: CapturableValue*): CaptureSet = CaptureSet(Set(values *))

  val empty: CaptureSet = CaptureSet(Set.empty)
  val singletonOfRoot: CaptureSet = CaptureSet(RootCapValue)

}
