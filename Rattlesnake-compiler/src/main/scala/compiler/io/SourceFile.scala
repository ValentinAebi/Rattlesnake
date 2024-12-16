package compiler.io

import compiler.gennames.FileExtensions

import scala.util.{Try, Using}

final case class SourceFile(path: String) extends SourceCodeProvider {
  require(path.endsWith(FileExtensions.rattlesnake))

  override def name: String = path

  override def lines: Try[Seq[String]] = {
    Using(scala.io.Source.fromFile(path)) { bufSrc =>
      bufSrc.getLines().toSeq
    }
  }

}
