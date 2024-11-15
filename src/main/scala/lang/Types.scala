package lang

import identifiers.TypeIdentifier
import lang.Captures.CaptureDescriptor
import lang.Types.PrimitiveType.NothingType


object Types {

  sealed trait Type {
    
    def maybeModifiable: Boolean
    def isModifiableForSure: Boolean
    
    def unmodifiable: Type

  }
  
  enum PrimitiveType(val str: String) extends Type {
    case IntType extends PrimitiveType("Int")
    case DoubleType extends PrimitiveType("Double")
    case CharType extends PrimitiveType("Char")
    case BoolType extends PrimitiveType("Bool")
    case StringType extends PrimitiveType("String")
    case RegionType extends PrimitiveType("Region")

    case VoidType extends PrimitiveType("Void")
    case NothingType extends PrimitiveType("Nothing")

    override def maybeModifiable: Boolean = false

    override def isModifiableForSure: Boolean = false
    override def unmodifiable: Type = this

    override def toString: String = str
  }
  
  def primTypeFor(name: TypeIdentifier): Option[PrimitiveType] = {
    PrimitiveType.values.find(_.str == name.stringId)
  }
  
  final case class NamedType(typeName: TypeIdentifier, captureDescr: CaptureDescriptor) extends Type {
    override def toString: String = typeName.stringId + captureDescr.toHatNotation
  }

  /**
   * Type of an array
   * @param elemType type of array elements
   * @param modifiable whether or not the content of the array is allowed to be updated from this reference
   */
  final case class ArrayType(elemType: Type, modifiable: Boolean) extends Type {

    override def maybeModifiable: Boolean = modifiable

    override def isModifiableForSure: Boolean = modifiable
    override def unmodifiable: Type = copy(modifiable = false)

    override def toString: String = {
      (if modifiable then (Keyword.Mut.str ++ " ") else "") ++ s"arr $elemType"
    }
  }
  
  final case class UnionType(unitedTypes: Set[Type]) extends Type {
    override def maybeModifiable: Boolean = unitedTypes.exists(_.maybeModifiable)
    override def isModifiableForSure: Boolean = unitedTypes.forall(_.isModifiableForSure)
    override def unmodifiable: Type = UnionType(unitedTypes.map(_.unmodifiable))

    override def toString: String = unitedTypes.toSeq.sortBy(_.toString).mkString(" | ")
  }

  /**
   * Type of a malformed/incorrect expression
   */
  case object UndefinedType extends Type {

    override def maybeModifiable: Boolean = true  // to avoid false positives with warnings for useless mut

    override def isModifiableForSure: Boolean = false
    override def unmodifiable: Type = this

    override def toString: String = "[undefined type]"
  }
  
}
