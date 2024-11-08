package compiler.backend

import compiler.AnalysisContext
import compiler.backend.DescriptorsCreator.descriptorForType
import identifiers.TypeIdentifier
import lang.Types.*
import lang.{StructSignature, Types}
import org.objectweb.asm
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.Type.*
import org.objectweb.asm.{Opcodes, Type}

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
      case _: (PrimitiveType.StringType.type | StructOrModuleType | ArrayType | UnionType) => None
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
  
  def internalNameOf(tpe: Types.Type)(using ctx: AnalysisContext): String = {
    tpe match
      case PrimitiveType.IntType => "I"
      case PrimitiveType.DoubleType => "D"
      case PrimitiveType.CharType => "C"
      case PrimitiveType.BoolType => "Z"
      case PrimitiveType.StringType => "java/lang/String"
      case PrimitiveType.VoidType => "V"
      case PrimitiveType.NothingType => "V"
      case StructOrModuleType(typeName, _) if !ctx.resolveType(typeName).get.isInterface => s"$typeName"
      case ArrayType(elemType, _) => s"[${descriptorForType(elemType)}"
      case StructOrModuleType(_, _) | UnionType(_) => "java/lang/Object"
      case UndefinedType => assert(false)
  }

  extension[T] (t: T) private def toSome = Some(t)

}
