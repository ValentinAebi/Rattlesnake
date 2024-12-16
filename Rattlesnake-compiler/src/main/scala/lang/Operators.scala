package lang

import identifiers.TypeIdentifier
import lang.Operator.*
import lang.Types.PrimitiveTypeShape.*
import lang.Types.TypeShape

import scala.annotation.targetName

object Operators {
  
  // Operators do not work on structs, hence the empty struct mapping
  private given Map[TypeIdentifier, StructSignature] = Map.empty

  /**
   * Signature of an unary operator
   */
  final case class UnaryOpSignature(op: Operator, operandType: TypeShape, retType: TypeShape)

  /**
   * Signature of a binary operator
   */
  final case class BinaryOpSignature(leftOperandType: TypeShape, op: Operator, rightOperandType: TypeShape, retType: TypeShape)

  // # is treated separately
  val unaryOperators: List[UnaryOpSignature] = List(
    UnaryOpSignature(Minus, IntType, IntType),
    UnaryOpSignature(Minus, DoubleType, DoubleType),
    UnaryOpSignature(ExclamationMark, BoolType, BoolType)
  )

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
    BoolType $ Or $ BoolType is BoolType,

    StringType $ Plus $ StringType is StringType
  )
  
  val assigOperators: Map[Operator, Operator] = Map(
    PlusEq -> Plus,
    MinusEq -> Minus,
    TimesEq -> Times,
    DivEq -> Div,
    ModuloEq -> Modulo
  )
  
  
  // Binop signature DSL implementation --------------------------------------------

  private case class PartialBinop1(leftOperandType: TypeShape, op: Operator) {
    @targetName("andThen") infix def $(rightOperandType: TypeShape): PartialBinop2 = {
      PartialBinop2(leftOperandType, op, rightOperandType)
    }
  }

  private case class PartialBinop2(leftOperandType: TypeShape, op: Operator, rightOperandType: TypeShape) {
    infix def is(retType: TypeShape): BinaryOpSignature = {
      BinaryOpSignature(leftOperandType, op, rightOperandType, retType)
    }
  }

  extension (leftOperandType: TypeShape) {
    @targetName("andThen") private infix def $(op: Operator): PartialBinop1 = {
      PartialBinop1(leftOperandType, op)
    }
  }

}
