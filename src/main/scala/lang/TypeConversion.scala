package lang

import lang.Types.PrimitiveTypeShape.*
import lang.Types.TypeShape

enum TypeConversion(val from: TypeShape, val to: TypeShape) {
  case Int2Double extends TypeConversion(IntType, DoubleType)
  case Double2Int extends TypeConversion(DoubleType, IntType)
  case IntToChar extends TypeConversion(IntType, CharType)
  case CharToInt extends TypeConversion(CharType, IntType)
}

object TypeConversion {
  
  def conversionFor(from: TypeShape, to: TypeShape): Option[TypeConversion] = {
    TypeConversion.values.find(conv => conv.from == from && conv.to == to)
  }
  
}
