package compiler.backend

import compiler.analysisctx.AnalysisContext
import identifiers.{IntrinsicsPackageId, NormalTypeId, TypeIdentifier}
import lang.Types.{PrimitiveType, Type}
import lang.*

object DescriptorsCreator {

  /**
   * @return JVM descriptor for [[tpe]]
   */
  def descriptorForType(tpe: Type)(using ctx: AnalysisContext): String = {
    tpe match
      case PrimitiveType.IntType => "I"
      case PrimitiveType.DoubleType => "D"
      case PrimitiveType.CharType => "C"
      case PrimitiveType.BoolType => "Z"
      case PrimitiveType.RegionType => "I"
      case PrimitiveType.StringType => "Ljava/lang/String;"
      case PrimitiveType.VoidType => "V"
      case PrimitiveType.NothingType => "V"
      case Types.NamedType(typeName) if !ctx.resolveType(typeName).get.isInterface => s"L$typeName;"
      case Types.NamedType(_) | Types.UnionType(_) => "Ljava/lang/Object;"
      case Types.ArrayType(elemType, _) => s"[${descriptorForType(elemType)}"
      case Types.UndefinedType => assert(false)
  }

  /**
   * @return JVM descriptor for [[funSig]]
   */
  def descriptorForFunc(funSig: FunctionSignature)(using AnalysisContext): String = {
    val FunctionSignature(_, argTypes, retType) = funSig
    argTypes.map(descriptorForType).mkString("(", "", ")") ++ descriptorForType(retType)
  }

}
