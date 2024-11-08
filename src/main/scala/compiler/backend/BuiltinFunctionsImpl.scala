package compiler.backend

import compiler.AnalysisContext
import compiler.backend.DescriptorsCreator.{descriptorForFunc, descriptorForType}
import compiler.irs.Asts.Expr
import identifiers.TypeIdentifier
import lang.Types.PrimitiveType.*
import lang.{Intrinsics, StructSignature}
import org.objectweb.asm.{MethodVisitor, Opcodes}

object BuiltinFunctionsImpl {

  def generatePrintCall(generateArg: () => Unit, mv: MethodVisitor): Unit = {
    mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "print", "(Ljava/lang/String;)V", false)
  }

  def generateIntToStringCall(generateArg: () => Unit, mv: MethodVisitor)
                             (using AnalysisContext): Unit = {
    generateToStringCallFor("Integer", descriptorForType(IntType), mv, generateArg)
  }

  def generateDoubleToStringCall(generateArg: () => Unit, mv: MethodVisitor)
                                (using AnalysisContext): Unit = {
    generateToStringCallFor("Double", descriptorForType(DoubleType), mv, generateArg)
  }

  def generateCharToStringCall(generateArg: () => Unit, mv: MethodVisitor)
                              (using AnalysisContext): Unit = {
    generateToStringCallFor("Character", descriptorForType(CharType), mv, generateArg)
  }

  def generateBoolToStringCall(generateArg: () => Unit, mv: MethodVisitor)
                              (using AnalysisContext): Unit = {
    generateToStringCallFor("Boolean", descriptorForType(BoolType), mv, generateArg)
  }

  def generateStringToCharArrayCall(generateArg: () => Unit, mv: MethodVisitor): Unit = {
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/String", "toCharArray",
      "()[C", false)
  }
  
  private def generateToStringCallFor(javaType: String, typeDescriptor: String, mv: MethodVisitor, generateArg: () => Unit): Unit = {
    generateArg()
    mv.visitMethodInsn(Opcodes.INVOKESTATIC, s"java/lang/$javaType", "toString",
      s"($typeDescriptor)Ljava/lang/String;", false)
  }

}
