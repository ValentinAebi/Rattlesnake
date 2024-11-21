package lang

import identifiers.TypeIdentifier


object Types {

  sealed trait Type
  
  enum PrimitiveType(val str: String) extends Type {
    case IntType extends PrimitiveType("Int")
    case DoubleType extends PrimitiveType("Double")
    case CharType extends PrimitiveType("Char")
    case BoolType extends PrimitiveType("Bool")
    case StringType extends PrimitiveType("String")
    case RegionType extends PrimitiveType("Region")

    case VoidType extends PrimitiveType("Void")
    case NothingType extends PrimitiveType("Nothing")

    override def toString: String = str
  }
  
  def primTypeFor(name: TypeIdentifier): Option[PrimitiveType] = {
    PrimitiveType.values.find(_.str == name.stringId)
  }
  
  final case class NamedType(typeName: TypeIdentifier) extends Type {
    override def toString: String = typeName.stringId
  }

  /**
   * Type of an array
   * @param elemType type of array elements
   * @param modifiable whether or not the content of the array is allowed to be updated from this reference
   */
  final case class ArrayType(elemType: Type, modifiable: Boolean) extends Type {
    override def toString: String = {
      (if modifiable then (Keyword.Mut.str ++ " ") else "") ++ s"arr $elemType"
    }
  }
  
  final case class UnionType(unitedTypes: Set[Type]) extends Type {
    override def toString: String = unitedTypes.toSeq.sortBy(_.toString).mkString(" | ")
  }

  /**
   * Type of a malformed/incorrect expression
   */
  case object UndefinedType extends Type {
    override def toString: String = "[undefined type]"
  }
  
}
