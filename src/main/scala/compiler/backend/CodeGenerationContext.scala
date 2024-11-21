package compiler.backend

import compiler.AnalysisContext
import compiler.backend.CodeGenerationContext.from
import compiler.backend.TypesConverter.numSlotsFor
import identifiers.{FunOrVarId, TypeIdentifier}
import lang.Types.PrimitiveType.*
import lang.Types.{PrimitiveType, Type}
import org.objectweb.asm.Label

import scala.annotation.tailrec
import scala.collection.mutable

final class CodeGenerationContext(
                                   val analysisContext: AnalysisContext,
                                   locals: List[mutable.Map[FunOrVarId, (Type, Int)]],
                                   var currLocalIdx: Int,
                                   val currentModule: TypeIdentifier
                                 ) {

  /**
   * @return a copy of this with a new empty local frame added on top of it
   */
  def withNewLocalsFrame: CodeGenerationContext = {
    new CodeGenerationContext(
      analysisContext,
      mutable.Map.empty[FunOrVarId, (Type, Int)] :: locals,
      currLocalIdx,
      currentModule
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

}

object CodeGenerationContext {

  def from(analysisContext: AnalysisContext, currentModule: TypeIdentifier): CodeGenerationContext = {
    new CodeGenerationContext(analysisContext, List(mutable.Map.empty[FunOrVarId, (Type, Int)]), 0, currentModule)
  }

}
