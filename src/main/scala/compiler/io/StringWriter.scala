package compiler.io

import compiler.Errors.{CompilationError, ErrorReporter}
import compiler.{CompilationStep, CompilerStep}

import java.io.FileWriter
import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Using}

final class StringWriter(directoryPath: Path, filename: String, errorReporter: ErrorReporter) extends CompilerStep[String, Unit] {
  
  override def apply(input: String): Unit = {
    Files.createDirectories(directoryPath)
    val path = directoryPath.resolve(filename)
    Files.createFile(path)
    val status = Using(new FileWriter(path.toFile))(_.write(input))
    status match
      case Failure(exception) =>
        errorReporter.push(new CompilationError(CompilationStep.SourceFileWriting, exception.getMessage, None))
      case Success(_) => ()
  }
  
}
