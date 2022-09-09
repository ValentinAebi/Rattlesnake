package lang

import lang.Operator.*
import lang.Types.PrimitiveType.*
import lang.Types.Type

import scala.annotation.targetName

object Operators {

  final case class UnaryOpSignature(op: Operator, operandType: Type, retType: Type)

  final case class BinaryOpSignature(leftOperandType: Type, op: Operator, rightOperandType: Type, retType: Type)

  // len is treated separately
  val unaryOperators: List[UnaryOpSignature] = List(
    UnaryOpSignature(Minus, IntType, IntType),
    UnaryOpSignature(Minus, DoubleType, DoubleType),
    UnaryOpSignature(ExclamationMark, BoolType, BoolType)
  )
  
  def unaryOpFor(operator: Operator, operand: Type): Option[UnaryOpSignature] = {
    unaryOperators.find {
      case UnaryOpSignature(op, operandType, _) =>
        operator == op && operandType == operand
    }
  }

  //  ==  and  !=  are treated separately
  val binaryOperators: List[BinaryOpSignature] = List(
    
    IntType $ Plus $ IntType is IntType,
    DoubleType $ Plus $ DoubleType is DoubleType,
    IntType $ Minus $ IntType is IntType,
    DoubleType $ Minus $ DoubleType is DoubleType,
    IntType $ Times $ IntType is IntType,
    DoubleType $ Times $ DoubleType is DoubleType,
    IntType $ Div $ IntType is IntType,
    DoubleType $ Div $ DoubleType is DoubleType,
    IntType $ Modulo $ IntType is IntType,
    
    IntType $ LessThan $ IntType is BoolType,
    DoubleType $ LessThan $ DoubleType is BoolType,
    IntType $ LessOrEq $ IntType is BoolType,
    DoubleType $ LessOrEq $ DoubleType is BoolType,
    IntType $ GreaterThan $ IntType is BoolType,
    DoubleType $ GreaterThan $ DoubleType is BoolType,
    IntType $ GreaterOrEq $ IntType is BoolType,
    DoubleType $ GreaterOrEq $ DoubleType is BoolType,

    BoolType $ And $ BoolType is BoolType,
    BoolType $ Or $ BoolType is BoolType
  )

  def binaryOpFor(left: Type, operator: Operator, right: Type): Option[BinaryOpSignature] = {
    binaryOperators.find {
      case BinaryOpSignature(leftOperandType, op, rightOperandType, _) =>
        leftOperandType == left && op == operator && rightOperandType == right
    }
  }
  
  val assigOperators: Map[Operator, Operator] = Map(
    PlusEq -> Plus,
    MinusEq -> Minus,
    TimesEq -> Times,
    DivEq -> Div,
    ModuloEq -> Modulo
  )

  private case class PartialBinop1(leftOperandType: Type, op: Operator) {
    @targetName("andThen") def $(rightOperandType: Type): PartialBinop2 = {
      PartialBinop2(leftOperandType, op, rightOperandType)
    }
  }

  private case class PartialBinop2(leftOperandType: Type, op: Operator, rightOperandType: Type) {
    def is(retType: Type): BinaryOpSignature = {
      BinaryOpSignature(leftOperandType, op, rightOperandType, retType)
    }
  }

  extension (leftOperandType: Type) {
    @targetName("andThen") private def $(op: Operator): PartialBinop1 = {
      PartialBinop1(leftOperandType, op)
    }
  }

}
