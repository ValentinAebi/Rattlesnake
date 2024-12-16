package compiler.backend

import compiler.analysisctx.AnalysisContext
import compiler.gennames.ClassesNames
import compiler.gennames.ClassesNames.runtimeClassName
import org.objectweb.asm.{MethodVisitor, Opcodes}

enum RuntimeMethod(name: String, mthDescr: String) {
  case StartPreparingEnvir extends RuntimeMethod("startPreparingEnvir", "()V")
  case AllowFilesystem extends RuntimeMethod("allowFilesystem", "()V")
  case PushEnvir extends RuntimeMethod("pushEnvir", "()V")
  case PopEnvir extends RuntimeMethod("popEnvir", "()V")
  case AssertFileSystemAllowed extends RuntimeMethod("assertFileSystemAllowed", "()V")
  case NewRegion extends RuntimeMethod("newRegion", "()I")
  
  def generateCall(mv: MethodVisitor)(using AnalysisContext): Unit = {
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, runtimeClassName, name, mthDescr, false)
  }
  
}
