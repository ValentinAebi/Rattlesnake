package compiler.backend

import compiler.AnalysisContext
import compiler.backend.CodeGenerationContext.from
import identifiers.FunOrVarId
import lang.Types.PrimitiveType.*
import lang.Types.{PrimitiveType, Type}
import org.objectweb.asm.Label

import scala.annotation.tailrec
import scala.collection.mutable

final class CodeGenerationContext(
                                   val analysisContext: AnalysisContext,
                                   locals: List[mutable.Map[FunOrVarId, (Type, Int)]],
                                   var currLocalIdx: Int,
                                   val functionStartLabel: Label
                                 ) {

  /**
   * @return a copy of this with a new empty local frame added on top of it
   */
  def withNewLocalsFrame: CodeGenerationContext = {
    new CodeGenerationContext(
      analysisContext,
      mutable.Map.empty[FunOrVarId, (Type, Int)] :: locals,
      currLocalIdx,
      functionStartLabel
    )
  }

  /**
   * Register a new local
   */
  def addLocal(name: FunOrVarId, tpe: Type): Unit = {
    locals.head.put(name, (tpe, currLocalIdx))
    currLocalIdx += numSlotsFor(tpe)
  }

  def getLocal(name: FunOrVarId): Option[(Type, Int)] = {

    @tailrec def searchLocal(remFrames: List[mutable.Map[FunOrVarId, (Type, Int)]]): Option[(Type, Int)] = {
      remFrames match {
        case Nil => None
        case head :: tail =>
          head.get(name) match {
            case None => searchLocal(tail)
            case some => some
          }
      }
    }

    searchLocal(locals)
  }

  export analysisContext.*

  private def numSlotsFor(tpe: Type): Int = {
    tpe match
      case DoubleType => 2
      case _ => 1
  }

}

object CodeGenerationContext {

  def from(analysisContext: AnalysisContext, functionStartLabel: Label): CodeGenerationContext = {
    new CodeGenerationContext(analysisContext, List(mutable.Map.empty[FunOrVarId, (Type, Int)]), 0, functionStartLabel)
  }

}
