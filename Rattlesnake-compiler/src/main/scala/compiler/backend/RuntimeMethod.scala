package compiler.backend

import compiler.analysisctx.AnalysisContext
import compiler.gennames.ClassesAndDirectoriesNames
import compiler.gennames.ClassesAndDirectoriesNames.runtimeClassName
import org.objectweb.asm.{MethodVisitor, Opcodes}

enum RuntimeMethod(name: String, mthDescr: String) {
  case SaveObjectInRegion extends RuntimeMethod("saveObjectInRegion", "(Ljava/lang/Object;I)V")
  case StartPreparingEnvir extends RuntimeMethod("startPreparingEnvir", "()V")
  case AllowFilesystem extends RuntimeMethod("allowFilesystem", "()V")
  case AllowRegion extends RuntimeMethod("allowRegion", "(I)V")
  case PushEnvir extends RuntimeMethod("pushEnvir", "()V")
  case PopEnvir extends RuntimeMethod("popEnvir", "()V")
  case NewRegion extends RuntimeMethod("newRegion", "()I")
  
  def generateCall(mv: MethodVisitor)(using AnalysisContext): Unit = {
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, runtimeClassName, name, mthDescr, false)
  }
  
}
