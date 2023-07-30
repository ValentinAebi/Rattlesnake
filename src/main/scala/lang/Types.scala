package lang

import identifiers.TypeIdentifier
import lang.Types.PrimitiveType.NothingType

/**
 * Subtyping rules: all types are independant (none is a supertype/subtype or another one), except that 
 * Nothing is a subtype of any type
 */
object Types {

  sealed trait Type {

    /**
     * @return `true` iff `this` is a subtype of `that`
     */
    def subtypeOf(that: Type): Boolean = {
      this == that || this == NothingType
    }

    /**
     * @return `true` iff one of `this` and `that` is a subtype of the other
     */
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

    override def toString: String = str
  }
  
  def primTypeFor(str: String): Option[PrimitiveType] = {
    PrimitiveType.values.find(_.str == str)
  }

  /**
   * Type of a structure instance
   */
  final case class StructType(typeName: TypeIdentifier) extends Type {
    override def toString: String = typeName.stringId
  }

  /**
   * Type of an array
   * @param elemType type of array elements
   */
  final case class ArrayType(elemType: Type) extends Type {
    override def toString: String = s"arr $elemType"
  }

  /**
   * Type of a malformed/incorrect expression
   */
  case object UndefinedType extends Type {
    override def toString: String = "[undefined type]"
  }
  
}
