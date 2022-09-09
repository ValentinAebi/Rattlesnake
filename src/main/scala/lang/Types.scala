package lang

import lang.Types.PrimitiveType.{AnyType, NothingType}

object Types {

  sealed trait Type {
    def subtypeOf(that: Type): Boolean = {
      this == that || this == NothingType || that == AnyType
    }
    
    def subtypeOrSupertype(that: Type): Boolean = {
      this.subtypeOf(that) || that.subtypeOf(this)
    }
  }
  
  enum PrimitiveType(val str: String) extends Type {
    case IntType extends PrimitiveType("Int")
    case DoubleType extends PrimitiveType("Double")
    case CharType extends PrimitiveType("Char")
    case BoolType extends PrimitiveType("Bool")
    case StringType extends PrimitiveType("String")
    case VoidType extends PrimitiveType("Void")

    case NothingType extends PrimitiveType("Nothing")
    case AnyType extends PrimitiveType("Any")

    override def toString: String = str
  }
  
  def primTypeFor(str: String): Option[PrimitiveType] = {
    PrimitiveType.values.find(_.str == str)
  }
  
  final case class StructType(typeName: String) extends Type {
    override def toString: String = typeName
  }
  
  final case class ArrayType(elemType: Type) extends Type {
    override def toString: String = s"arr $elemType"
  }
  
}
