package compiler

import scala.util.{Try, Using}

final case class SourceFile(path: String) extends SourceCodeProvider {
  override def name: String = path
  
  override def lines: Try[Seq[String]] = {
    Using(scala.io.Source.fromFile(path)){ bufSrc =>
      bufSrc.getLines().toSeq
    }
  }
  
}
