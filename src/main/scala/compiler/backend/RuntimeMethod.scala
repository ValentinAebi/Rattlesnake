package compiler.backend

import compiler.analysisctx.AnalysisContext
import compiler.gennames.NamesForGeneratedClasses.runtimeClassName
import org.objectweb.asm.{MethodVisitor, Opcodes}

enum RuntimeMethod(name: String, mthDescr: String) {
  case NewRegion extends RuntimeMethod("newRegion", "()I")
  case SaveNewObjectInRegion extends RuntimeMethod("saveNewObjectInRegion", "(Ljava/lang/Object;I)V")
  case AddAllowedResource extends RuntimeMethod("addAllowedResource", "(I)V")
  case PushFrame extends RuntimeMethod("pushFrame", "()V")
  case PopFrame extends RuntimeMethod("popFrame", "()V")
  case AssertAllowed extends RuntimeMethod("assertAllowed", "(I)V")
  
  def generateCall(mv: MethodVisitor)(using AnalysisContext): Unit = {
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, runtimeClassName, name, mthDescr, false)
  }
  
}
