package compiler

import scala.collection.mutable

final class PrettyPrintString(indentGranularity: Int) {
  require(indentGranularity > 0)
  
  private var currIndentLevel: Int = 0
  private val lines = new mutable.ListBuffer[String]()

  def incrementIndent(): PrettyPrintString = {
    currIndentLevel += 1
    this
  }

  def decrementIndent(): PrettyPrintString = {
    currIndentLevel -= 1
    this
  }

  def newLine(): PrettyPrintString = {
    lines.addOne(indentSpaces)
    this
  }

  def add(s: String): PrettyPrintString = {
    if (s.nonEmpty){
      val newLines = scala.io.Source.fromString(s).getLines()
      if (newLines.hasNext) {
        if (lines.nonEmpty){
          val lastLineIdx = lines.size - 1
          lines.update(lastLineIdx, lines(lastLineIdx) ++ newLines.next())
        }
        else {
          lines.addOne(newLines.next())
        }
      }
      lines.addAll(newLines.map(indentSpaces ++ _))
    }
    this
  }

  def startBlock(): PrettyPrintString = {
    add("{")
    incrementIndent()
  }

  def endBlock(): PrettyPrintString = {
    decrementIndent()
    newLine()
    add("}")
    this
  }

  def addSpace(): PrettyPrintString = {
    add(" ")
    this
  }

  def built: String = lines.mkString("\n")

  private def indentSpaces: String = " ".repeat(indentGranularity * currIndentLevel)

}
