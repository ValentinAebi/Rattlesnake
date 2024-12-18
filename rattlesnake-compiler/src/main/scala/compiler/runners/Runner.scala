package compiler.runners

import compiler.gennames.ClassesAndDirectoriesNames.agentSubdirName

import java.io.File
import java.nio.file.Path

final class Runner(errorCallback: String => Nothing) {

  private val classPathsSep =
    if System.getProperty("os.name").startsWith("Windows")
    then ";"
    else ":"

  def runMain(workingDirectoryPath: Path, mainClassName: String): Int = {
    val agentSubdirPath = workingDirectoryPath.resolve(agentSubdirName)
    val agentJarName = findNameOfJarInDir(agentSubdirPath.toFile, "Rattlesnake-agent",
      "Rattlesnake agent not found")
    val runtimeJarName = findNameOfJarInDir(workingDirectoryPath.toFile, "Rattlesnake-runtime",
      "Rattlesnake runtime not found")
    val agentJarFullPath = agentSubdirPath.resolve(agentJarName).toFile.getCanonicalFile
    val runtimeJarFullPath = workingDirectoryPath.resolve(runtimeJarName).toFile.getCanonicalFile
    new ProcessBuilder()
      .directory(workingDirectoryPath.toFile)
      .inheritIO()
      .command(
        "java",
        "-cp", s"\"$runtimeJarFullPath$classPathsSep.\"",
        s"-javaagent:$agentJarFullPath",
        mainClassName
      ).start()
      .waitFor()
  }

  private def findNameOfJarInDir(dir: File, jarNamePrefix: String, errorMsg: String): String = {
    dir.list().find(f => f.startsWith(jarNamePrefix) && f.endsWith("with-dependencies.jar"))
      .getOrElse {
        errorCallback(errorMsg)
      }
  }

}
