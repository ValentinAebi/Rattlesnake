package compiler.capturechecker

import lang.CapturableValue

import scala.collection.mutable

final class CapturesTracker {
  private val values: mutable.Map[CapturableValue, CaptureDescriptor] = mutable.Map.empty
  
  export values.{apply, update}

}
