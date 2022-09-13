package compiler.backend

import compiler.irs.Asts.Expr
import org.objectweb.asm.{MethodVisitor, Opcodes}

object BuiltinFunctionsImpl {

  def generatePrintCall(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "print", "(Ljava/lang/String;)V", false)
  }

  def generateIntToString(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Integer", mv, generateArg)
  }

  def generateDoubleToString(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Double", mv, generateArg)
  }

  def generateCharToString(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Character", mv, generateArg)
  }

  def generateBoolToString(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Boolean", mv, generateArg)
  }
  
  private def generateToStringFor(javaType: String, mv: MethodVisitor, generateArg: () => Unit): Unit = {
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, s"java/lang/$javaType", "toString", "(I)Ljava/lang/String;", false)
  }

}
