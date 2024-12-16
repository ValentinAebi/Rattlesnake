package compiler.gennames

import lang.Device

object ClassesAndDirectoriesNames {

  val constantsClassName: String = "$constants"

  val packageInstanceName: String = "$INSTANCE"
  
  val runtimeClassName: String = "Rattlesnake$runtime"
  val fileSystemClassName: String = Device.FileSystem.typeName.stringId
  
  val outDirName: String = "out"
  val agentSubdirName: String = "agent"
  
}
