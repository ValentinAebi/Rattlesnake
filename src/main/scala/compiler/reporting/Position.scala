package compiler.reporting

import compiler.io.SourceCodeProvider

/**
 * Position in a source (typically a source file)
 *
 * Ordered according to lexicographic ordering of srcCodeProviderName, then line, and finally column
 */
final case class Position(srcCodeProviderName: String, line: Int, col: Int) extends Ordered[Position] {
  require(srcCodeProviderName != null)
  require(line >= 1)
  require(col >= 1)
  
  def shiftedRightOf(n: Int): Position = copy(col = col + n)


  override def compare(that: Position): Int = {
    val fileNamesComp = this.srcCodeProviderName.compareTo(that.srcCodeProviderName)
    val lineComp = this.line.compareTo(that.line)
    val colComp = this.col.compareTo(that.col)
    if fileNamesComp != 0 then fileNamesComp
    else if lineComp != 0 then lineComp
    else colComp
  }

  override def toString: String = s"$srcCodeProviderName:$line:$col"

}

object Position {

  def apply(srcCodeProvider: SourceCodeProvider, line: Int, col: Int): Position = {
    new Position(srcCodeProvider.name, line, col)
  }

}
