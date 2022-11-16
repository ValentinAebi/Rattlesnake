package compiler.desugarer

import java.util.concurrent.atomic.AtomicLong

final class UniqueIdGenerator {
  private val counter = new AtomicLong(0)
  
  def next(): String = {
    val idx = counter.getAndIncrement()
    "$" + idx
  }

}
