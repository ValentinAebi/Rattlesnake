name := "Rattlesnake"

version := "0.1"

scalaVersion := "3.5.0"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
libraryDependencies += "org.ow2.asm" % "asm" % "9.3"
libraryDependencies += "org.ow2.asm" % "asm-util" % "9.3"

Compile / unmanagedResourceDirectories += baseDirectory.value / "resources"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
