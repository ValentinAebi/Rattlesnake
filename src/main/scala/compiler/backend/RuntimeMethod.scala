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
  case AssertResourceAllowed extends RuntimeMethod("assertResourceAllowed", "(I)V")
  case AssertCanModifyRegionOf extends RuntimeMethod("assertCanModifyRegionOf", "(Ljava/lang/Object;)V")
  case AssertFilesystemAllowed extends RuntimeMethod("assertFilesystemAllowed", "()V")
  
  def generateCall(mv: MethodVisitor)(using AnalysisContext): Unit = {
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, runtimeClassName, name, mthDescr, false)
  }
  
}
