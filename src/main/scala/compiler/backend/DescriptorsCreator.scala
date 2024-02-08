package compiler.backend

import identifiers.TypeIdentifier
import lang.Types.{PrimitiveType, Type}
import lang.{FunctionSignature, StructSignature, Types}

object DescriptorsCreator {

  /**
   * @return JVM descriptor for [[tpe]]
   */
  def descriptorForType(tpe: Type)(using structs: Map[TypeIdentifier, StructSignature]): String = {
    tpe match
      case PrimitiveType.IntType => "I"
      case PrimitiveType.DoubleType => "D"
      case PrimitiveType.CharType => "C"
      case PrimitiveType.BoolType => "Z"
      case PrimitiveType.StringType => "Ljava/lang/String;"
      case PrimitiveType.VoidType => "V"
      case PrimitiveType.NothingType => "V"
      case Types.StructType(typeName, _) if !structs.apply(typeName).isInterface => s"L$typeName;"
      case Types.ArrayType(elemType, _) => s"[${descriptorForType(elemType)}"
      // TODO (maybe) optimization with a main supertype compiled to an abstract class
      case Types.StructType(_, _) | Types.UnionType(_) => "Ljava/lang/Object;"
      case Types.UndefinedType => assert(false)
  }

  /**
   * @return JVM descriptor for [[funSig]]
   */
  def descriptorForFunc(funSig: FunctionSignature)(using Map[TypeIdentifier, StructSignature]): String = {
    val FunctionSignature(_, argTypes, retType) = funSig
    argTypes.map(descriptorForType).mkString("(", "", ")") ++ descriptorForType(retType)
  }

}
