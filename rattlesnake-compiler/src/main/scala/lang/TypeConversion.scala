package lang

import lang.Types.PrimitiveTypeShape.*
import lang.Types.{Type, TypeShape}

enum TypeConversion(val from: TypeShape, val to: TypeShape) {
  case Int2Double extends TypeConversion(IntType, DoubleType)
  case Double2Int extends TypeConversion(DoubleType, IntType)
  case IntToChar extends TypeConversion(IntType, CharType)
  case CharToInt extends TypeConversion(CharType, IntType)
}

object TypeConversion {
  
  def conversionFor(from: Type, to: Type): Option[TypeConversion] = {
    TypeConversion.values.find(conv => conv.from.shape == from && conv.to == to.shape)
  }
  
}
