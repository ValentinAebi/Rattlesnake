package compiler.backend

import compiler.analysisctx.AnalysisContext
import identifiers.{IntrinsicsPackageId, NormalTypeId, TypeIdentifier}
import lang.Types.{PrimitiveTypeShape, TypeShape}
import lang.*

object DescriptorsCreator {

  /**
   * @return JVM descriptor for [[tpe]]
   */
  def descriptorForType(tpe: TypeShape)(using ctx: AnalysisContext): String = {
    tpe match
      case PrimitiveTypeShape.IntType => "I"
      case PrimitiveTypeShape.DoubleType => "D"
      case PrimitiveTypeShape.CharType => "C"
      case PrimitiveTypeShape.BoolType => "Z"
      case PrimitiveTypeShape.RegionType => "I"
      case PrimitiveTypeShape.StringType => "Ljava/lang/String;"
      case PrimitiveTypeShape.VoidType => "V"
      case PrimitiveTypeShape.NothingType => "V"
      case Types.NamedTypeShape(typeName) if !ctx.resolveType(typeName).get.isInterface => s"L$typeName;"
      case Types.NamedTypeShape(_) | Types.UnionTypeShape(_) => "Ljava/lang/Object;"
      case Types.ArrayTypeShape(elemType, _) => s"[${descriptorForType(elemType)}"
      case Types.UndefinedTypeShape => assert(false)
  }

  /**
   * @return JVM descriptor for [[funSig]]
   */
  def descriptorForFunc(funSig: FunctionSignature)(using AnalysisContext): String = {
    val FunctionSignature(_, argTypes, retType) = funSig
    argTypes.map(descriptorForType).mkString("(", "", ")") ++ descriptorForType(retType)
  }

}
