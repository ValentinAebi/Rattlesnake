package compiler.backend

import compiler.backend.DescriptorsCreator.{descriptorForFunc, descriptorForType}
import compiler.irs.Asts.Expr
import identifiers.TypeIdentifier
import lang.Types.PrimitiveType.*
import lang.{BuiltInFunctions, StructSignature}
import org.objectweb.asm.{MethodVisitor, Opcodes}

object BuiltinFunctionsImpl {

  def generatePrintCall(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "print", "(Ljava/lang/String;)V", false)
  }

  def generateIntToString(generateArg: () => Unit)
                         (using Map[TypeIdentifier, StructSignature])
                         (implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Integer", descriptorForType(IntType), mv, generateArg)
  }

  def generateDoubleToString(generateArg: () => Unit)
                            (using Map[TypeIdentifier, StructSignature])
                            (implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Double", descriptorForType(DoubleType), mv, generateArg)
  }

  def generateCharToString(generateArg: () => Unit)
                          (using Map[TypeIdentifier, StructSignature])
                          (implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Character", descriptorForType(CharType), mv, generateArg)
  }

  def generateBoolToString(generateArg: () => Unit)
                          (using Map[TypeIdentifier, StructSignature])
                          (implicit mv: MethodVisitor): Unit = {
    generateToStringFor("Boolean", descriptorForType(BoolType), mv, generateArg)
  }

  def generateStringToCharArray(generateArg: () => Unit)(implicit mv: MethodVisitor): Unit = {
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/String", "toCharArray",
      "()[C", false)
  }
  
  private def generateToStringFor(javaType: String, typeDescriptor: String, mv: MethodVisitor, generateArg: () => Unit): Unit = {
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, s"java/lang/$javaType", "toString",
      s"($typeDescriptor)Ljava/lang/String;", false)
  }

}
