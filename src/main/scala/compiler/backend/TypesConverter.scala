package compiler.backend

import lang.Types
import lang.Types.{ArrayType, PrimitiveType, StructType}
import org.objectweb.asm
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type.*

object TypesConverter {

  def opcodeFor(tpe: Types.Type, intOpcode: Int, refOpcode: => Int): Int = {
    convertToAsmType(tpe).map(_.getOpcode(intOpcode)).getOrElse(refOpcode)
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
  }

  def convertToAsmTypeCode(tpe: Types.Type): Option[Int] = {
    tpe match
      case PrimitiveType.IntType => Opcodes.T_INT.toSome
      case PrimitiveType.DoubleType => Opcodes.T_DOUBLE.toSome
      case PrimitiveType.CharType => Opcodes.T_CHAR.toSome
      case PrimitiveType.BoolType => Opcodes.T_BOOLEAN.toSome
      case _ => None
  }

  extension[T](t: T) private def toSome = Some(t)

}
