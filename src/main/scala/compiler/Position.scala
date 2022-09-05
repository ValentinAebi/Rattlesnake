package compiler

/**
 * Position in a source (typically a source file)
 */
final case class Position(srcCodeProviderName: String, line: Int, col: Int){
  require(srcCodeProviderName != null)
  require(line >= 1)
  require(col >= 1)

  override def toString: String = s"$srcCodeProviderName:$line:$col"

}

object Position {

  def apply(srcCodeProvider: SourceCodeProvider, line: Int, col: Int): Position = {
    new Position(srcCodeProvider.name, line, col)
  }

}
