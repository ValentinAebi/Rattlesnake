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
    def subtypeOf(that: Type): Boolean

    /**
     * @return `true` iff one of `this` and `that` is a subtype of the other
     */
    def subtypeOrSupertype(that: Type): Boolean = {
      this.subtypeOf(that) || that.subtypeOf(this)
    }

    def isModifiable: Boolean

  }
  
  enum PrimitiveType(val str: String) extends Type {
    case IntType extends PrimitiveType("Int")
    case DoubleType extends PrimitiveType("Double")
    case CharType extends PrimitiveType("Char")
    case BoolType extends PrimitiveType("Bool")
    case StringType extends PrimitiveType("String")

    case VoidType extends PrimitiveType("Void")
    case NothingType extends PrimitiveType("Nothing")

    override def subtypeOf(that: Type): Boolean = {
      this == that || this == NothingType
    }

    override def isModifiable: Boolean = false

    override def toString: String = str
  }
  
  def primTypeFor(name: TypeIdentifier): Option[PrimitiveType] = {
    PrimitiveType.values.find(_.str == name.stringId)
  }

  /**
   * Type of a structure instance
   * @param modifiable whether or not the fields of the struct are allowed to be modified from this reference
   */
  final case class StructType(typeName: TypeIdentifier, modifiable: Boolean) extends Type {

    override def subtypeOf(that: Type): Boolean = {
      that match
        case that: StructType =>
          that.typeName == this.typeName && logicalImplies(that.modifiable, this.modifiable)
        case _ => false
    }

    override def isModifiable: Boolean = modifiable

    override def toString: String = {
      (if modifiable then (Keyword.Mut.str ++ " ") else "") ++ typeName.stringId
    }
  }

  /**
   * Type of an array
   * @param elemType type of array elements
   * @param modifiable whether or not the content of the array is allowed to be updated from this reference
   */
  final case class ArrayType(elemType: Type, modifiable: Boolean) extends Type {

    override def subtypeOf(that: Type): Boolean = {
      that match
        case that: ArrayType =>
          that.elemType == this.elemType && logicalImplies(that.modifiable, this.modifiable)
        case _ => false
    }

    override def isModifiable: Boolean = modifiable

    override def toString: String = {
      (if modifiable then (Keyword.Mut.str ++ " ") else "") ++ s"arr $elemType"
    }
  }

  /**
   * Type of a malformed/incorrect expression
   */
  case object UndefinedType extends Type {

    override def subtypeOf(that: Type): Boolean = true  // otherwise may yield a cascade of error messages in type-checking

    override def isModifiable: Boolean = false

    override def toString: String = "[undefined type]"
  }

  private def logicalImplies(a: Boolean, b: Boolean): Boolean = (!a || b)
  
}
