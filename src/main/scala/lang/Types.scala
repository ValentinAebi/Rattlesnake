package lang

import identifiers.TypeIdentifier
import lang.CaptureDescriptors.*

import scala.annotation.targetName


object Types {
  
  sealed trait Type {
    def shape: TypeShape
    def captureDescriptor: CaptureDescriptor
  }
  
  final case class CapturingType(shape: TypeShape, captureDescriptor: CaptureDescriptor) extends Type {
    override def toString: String =
      if captureDescriptor.isEmpty then shape.toString
      else if captureDescriptor.isRoot then s"$shape^"
      else s"$shape^$captureDescriptor"
  }

  sealed trait TypeShape extends Type {
    override def shape: TypeShape = this
    override def captureDescriptor: CaptureDescriptor = CaptureSet.empty
    @targetName("capturing") infix def ^(cd: CaptureDescriptor): CapturingType = CapturingType(this, cd)
    @targetName("maybeCapturing") infix def ^(cdOpt: Option[CaptureDescriptor]): Type =
      cdOpt.map(CapturingType(this, _)).getOrElse(this)
  }
  
  enum PrimitiveTypeShape(val str: String) extends TypeShape {
    case IntType extends PrimitiveTypeShape("Int")
    case DoubleType extends PrimitiveTypeShape("Double")
    case CharType extends PrimitiveTypeShape("Char")
    case BoolType extends PrimitiveTypeShape("Bool")
    case StringType extends PrimitiveTypeShape("String")
    case RegionType extends PrimitiveTypeShape("Region")

    case VoidType extends PrimitiveTypeShape("Void")
    case NothingType extends PrimitiveTypeShape("Nothing")

    override def toString: String = str
  }
  
  def primTypeFor(name: TypeIdentifier): Option[PrimitiveTypeShape] = {
    PrimitiveTypeShape.values.find(_.str == name.stringId)
  }
  
  final case class NamedTypeShape(typeName: TypeIdentifier) extends TypeShape {
    override def toString: String = typeName.stringId
  }

  /**
   * Type of an array
   * @param elemType type of array elements
   * @param modifiable whether or not the content of the array is allowed to be updated from this reference
   */
  final case class ArrayTypeShape(elemType: Type, modifiable: Boolean) extends TypeShape {
    override def toString: String = {
      (if modifiable then (Keyword.Mut.str ++ " ") else "") ++ s"arr $elemType"
    }
  }
  
  final case class UnionTypeShape(unitedTypes: Set[TypeShape]) extends TypeShape {
    override def toString: String = unitedTypes.toSeq.sortBy(_.toString).mkString(" | ")
  }

  /**
   * Type of a malformed/incorrect expression
   */
  case object UndefinedTypeShape extends TypeShape {
    override def toString: String = "[undefined type]"
  }
  
}
