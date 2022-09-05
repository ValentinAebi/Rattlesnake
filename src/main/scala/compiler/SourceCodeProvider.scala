package compiler

import scala.util.Try

/**
 * Object able to provide source code (typically a source file)
 */
trait SourceCodeProvider {
  def lines: Try[Seq[String]]
  def name: String

  override def toString: String = name
}
