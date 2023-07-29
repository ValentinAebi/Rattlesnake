package compiler.lowerer

import identifiers.LowererGeneratedVarId

import java.util.concurrent.atomic.AtomicLong

final class UniqueIdGenerator {
  private val counter = new AtomicLong(0)
  
  def next(): LowererGeneratedVarId = {
    val idx = counter.getAndIncrement()
    LowererGeneratedVarId("$" + idx)
  }

}
