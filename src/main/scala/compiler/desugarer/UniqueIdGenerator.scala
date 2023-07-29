package compiler.desugarer

import identifiers.DesugarerGeneratedVarId

import java.util.concurrent.atomic.AtomicLong

final class UniqueIdGenerator {
  private val counter = new AtomicLong(0)
  
  def next(): DesugarerGeneratedVarId = {
    val idx = counter.getAndIncrement()
    DesugarerGeneratedVarId("$" + idx)
  }

}
