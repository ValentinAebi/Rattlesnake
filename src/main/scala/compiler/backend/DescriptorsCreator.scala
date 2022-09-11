package compiler.backend

import lang.{FunctionSignature, Types}
import lang.Types.{PrimitiveType, Type}

object DescriptorsCreator {

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
  }

  def descriptorForFunc(funSig: FunctionSignature): String = {
    val FunctionSignature(_, argTypes, retType) = funSig
    argTypes.map(descriptorForType).mkString("(", "", ")") ++ descriptorForType(retType)
  }

}
