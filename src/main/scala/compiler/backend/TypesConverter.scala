package compiler.backend

import lang.Types
import lang.Types.{ArrayType, PrimitiveType, StructType}
import org.objectweb.asm
import org.objectweb.asm.{Opcodes, Type}
import org.objectweb.asm.Type.*
import Opcodes.*
import DescriptorsCreator.descriptorForType

object TypesConverter {

  private def safeGetOpcode(tpe: Type, intOpcode: Int): Int = {
    val allowedOpcodes = Set(ILOAD, ISTORE, IALOAD, IASTORE, IADD, ISUB, IMUL, IDIV, IREM, INEG, ISHL, ISHR, IUSHR, IAND, IOR, IXOR, IRETURN)
    require(allowedOpcodes.contains(intOpcode), s"unexpected opcode: $intOpcode")
    tpe.getOpcode(intOpcode)
  }

  def opcodeFor(tpe: Types.Type, intOpcode: Int, refOpcode: => Int): Int = {
    convertToAsmType(tpe).map(safeGetOpcode(_, intOpcode)).getOrElse(refOpcode)
  }

  def convertToAsmType(tpe: Types.Type): Option[asm.Type] = {
    tpe match
      case PrimitiveType.IntType => INT_TYPE.toSome
      case PrimitiveType.DoubleType => DOUBLE_TYPE.toSome
      case PrimitiveType.CharType => CHAR_TYPE.toSome
      case PrimitiveType.BoolType => BOOLEAN_TYPE.toSome
      case PrimitiveType.VoidType => VOID_TYPE.toSome
      case PrimitiveType.NothingType => VOID_TYPE.toSome
      case _: (PrimitiveType.StringType.type | StructType | ArrayType) => None
      case Types.UndefinedType => assert(false)
  }

  def convertToAsmTypeCode(tpe: Types.Type): Option[Int] = {
    tpe match
      case PrimitiveType.IntType => Opcodes.T_INT.toSome
      case PrimitiveType.DoubleType => Opcodes.T_DOUBLE.toSome
      case PrimitiveType.CharType => Opcodes.T_CHAR.toSome
      case PrimitiveType.BoolType => Opcodes.T_BOOLEAN.toSome
      case _ => None
  }
  
  def internalNameOf(tpe: Types.Type): String = {
    tpe match
      case PrimitiveType.IntType => "I"
      case PrimitiveType.DoubleType => "D"
      case PrimitiveType.CharType => "C"
      case PrimitiveType.BoolType => "Z"
      case PrimitiveType.StringType => "java/lang/String"
      case PrimitiveType.VoidType => "V"
      case PrimitiveType.NothingType => "V"
      case Types.StructType(typeName) => s"$typeName"
      case Types.ArrayType(elemType) => s"[${descriptorForType(elemType)}"
      case Types.UndefinedType => assert(false)
  }

  extension[T] (t: T) private def toSome = Some(t)

}
