package compiler.backend

import compiler.AnalysisContext
import compiler.backend.CodeGenerationContext.from
import lang.Types.Type

import scala.annotation.tailrec
import scala.collection.mutable

final class CodeGenerationContext(val analysisContext: AnalysisContext, locals: List[mutable.Map[String, (Type, Int)]], var currLocalIdx: Int) {

  def withNewLocalsFrame: CodeGenerationContext = {
    new CodeGenerationContext(analysisContext, mutable.Map.empty[String, (Type, Int)] :: locals, currLocalIdx)
  }

  def addLocal(name: String, tpe: Type): Unit = {
    locals.head.put(name, (tpe, currLocalIdx))
    currLocalIdx += 1
  }

  def getLocal(name: String): (Type, Int) = {

    @tailrec def recurse(remFrames: List[mutable.Map[String, (Type, Int)]]): (Type, Int) = {
      remFrames match {
        case Nil => throw new NoSuchElementException(s"should not happen: local $name not found")
        case head :: tail =>
          head.get(name) match {
            case Some(value) => value
            case None => recurse(tail)
          }
      }
    }

    recurse(locals)
  }

  export analysisContext.*

}

object CodeGenerationContext {

  def from(analysisContext: AnalysisContext): CodeGenerationContext = {
    new CodeGenerationContext(analysisContext, List(mutable.Map.empty[String, (Type, Int)]), 0)
  }

}
