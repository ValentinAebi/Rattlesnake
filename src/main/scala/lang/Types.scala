package lang

import identifiers.TypeIdentifier
import lang.Captures.{Brand, CaptureDescriptor, CaptureSet}
import lang.Types.PrimitiveType.NothingType


object Types {

  sealed trait Type {
    
    def maybeModifiable: Boolean
    def isModifiableForSure: Boolean
    
    def unmodifiable: Type
    
    def captureDescr: CaptureDescriptor
    def isPure: Boolean = captureDescr.isCapSetOfPureType
    
    def equalsIgnoringCaptures(that: Type): Boolean = (this, that) match {
      case (NamedType(tid1, cd1), NamedType(tid2, cd2)) => tid1 == tid2
      case _ => this == that
    }

  }
  
  enum PrimitiveType(val str: String, override val captureDescr: CaptureDescriptor) extends Type {
    case IntType extends PrimitiveType("Int", CaptureSet.empty)
    case DoubleType extends PrimitiveType("Double", CaptureSet.empty)
    case CharType extends PrimitiveType("Char", CaptureSet.empty)
    case BoolType extends PrimitiveType("Bool", CaptureSet.empty)
    case StringType extends PrimitiveType("String", CaptureSet.empty)
    case RegionType extends PrimitiveType("Region", CaptureSet.singletonOfRoot)

    case VoidType extends PrimitiveType("Void", CaptureSet.empty)
    case NothingType extends PrimitiveType("Nothing", CaptureSet.empty)

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

    override def captureDescr: CaptureDescriptor = CaptureSet.empty

    override def toString: String = {
      (if modifiable then (Keyword.Mut.str ++ " ") else "") ++ s"arr $elemType"
    }
  }
  
  final case class UnionType(unitedTypes: Set[Type]) extends Type {
    override def maybeModifiable: Boolean = unitedTypes.exists(_.maybeModifiable)
    override def isModifiableForSure: Boolean = unitedTypes.forall(_.isModifiableForSure)
    override def unmodifiable: Type = UnionType(unitedTypes.map(_.unmodifiable))

    override def captureDescr: CaptureDescriptor = unitedTypes.foldLeft[CaptureDescriptor](CaptureSet.empty){
      (accCs, currType) => accCs.union(currType.captureDescr)
    }

    override def toString: String = unitedTypes.toSeq.sortBy(_.toString).mkString(" | ")
  }

  /**
   * Type of a malformed/incorrect expression
   */
  case object UndefinedType extends Type {

    override def maybeModifiable: Boolean = true  // to avoid false positives with warnings for useless mut

    override def isModifiableForSure: Boolean = false
    override def unmodifiable: Type = this

    override def captureDescr: CaptureDescriptor = Brand

    override def toString: String = "[undefined type]"
  }
  
}
