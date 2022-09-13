package compiler.backend

import compiler.backend.DescriptorsCreator.descriptorForType
import compiler.irs.Asts.Expr
import lang.Types.PrimitiveType.*
import org.objectweb.asm.{MethodVisitor, Opcodes}

object BuiltinFunctionsImpl {

  def generatePrintCall(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "print", "(Ljava/lang/String;)V", false)
  }

  def generateIntToString(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Integer", descriptorForType(IntType), mv, generateArg)
  }

  def generateDoubleToString(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Double", descriptorForType(DoubleType), mv, generateArg)
  }

  def generateCharToString(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Character", descriptorForType(CharType), mv, generateArg)
  }

  def generateBoolToString(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Boolean", descriptorForType(BoolType), mv, generateArg)
  }
  
  private def generateToStringFor(javaType: String, typeDescriptor: String, mv: MethodVisitor, generateArg: () => Unit): Unit = {
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, s"java/lang/$javaType", "toString",
      s"($typeDescriptor)Ljava/lang/String;", false)
  }

}
