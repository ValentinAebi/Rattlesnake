package compiler.backend

import lang.Types.{PrimitiveType, Type}
import lang.{FunctionSignature, Types}

object DescriptorsCreator {

  /**
   * @return JVM descriptor for [[tpe]]
   */
  def descriptorForType(tpe: Type): String = {
    tpe match
      case PrimitiveType.IntType => "I"
      case PrimitiveType.DoubleType => "D"
      case PrimitiveType.CharType => "C"
      case PrimitiveType.BoolType => "Z"
      case PrimitiveType.StringType => "Ljava/lang/String;"
      case PrimitiveType.VoidType => "V"
      case PrimitiveType.NothingType => "V"
      case Types.StructType(typeName) => s"L$typeName;"
      case Types.ArrayType(elemType) => s"[${descriptorForType(elemType)}"
      case Types.UndefinedType => assert(false)
  }

  /**
   * @return JVM descriptor for [[funSig]]
   */
  def descriptorForFunc(funSig: FunctionSignature): String = {
    val FunctionSignature(_, argTypes, retType) = funSig
    argTypes.map(descriptorForType).mkString("(", "", ")") ++ descriptorForType(retType)
  }

}
