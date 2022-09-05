package compiler.irs

import compiler.Position
import lang.Operator
import lang.Types.*

object Asts {

  sealed abstract class Ast {
    // Positions are propagated by the TreeParser
    // Each AST is assigned the position of its leftmost token (by the map method of TreeParser)
    // For ASTs that are unreachable by this system, position is set by their constructor using their leftmost child
    private var positionOpt: Option[Position] = None

    final def setPosition(posOpt: Option[Position]): Unit = {
      positionOpt = posOpt
    }

    final def getPosition: Option[Position] = positionOpt
  }

  sealed abstract class Statement extends Ast
  sealed abstract class Expr extends Statement

  final case class Source(defs: List[TopLevelDef]) extends Ast
  final case class Block(stats: List[Statement]) extends Statement

  sealed abstract class TopLevelDef extends Ast
  final case class FunDef(funName: String, args: List[Param], optRetType: Option[Type], body: Block) extends TopLevelDef
  final case class StructDef(structName: String, fields: List[Param]) extends TopLevelDef
  final case class Param(paramName: String, tpe: Type) extends Ast

  final case class ValDef(valName: String, optType: Option[Type], rhs: Expr) extends Statement
  final case class VarDef(varName: String, optType: Option[Type], rhs: Expr) extends Statement

  sealed abstract class Literal extends Expr {
    val value: Any
  }
  final case class IntLit(value: Int) extends Literal
  final case class DoubleLit(value: Double) extends Literal
  final case class CharLit(value: Char) extends Literal
  final case class BoolLit(value: Boolean) extends Literal
  final case class StringLit(value: String) extends Literal

  final case class VariableRef(name: String) extends Expr
  final case class Call(callee: Expr, args: List[Expr]) extends Expr
  final case class Indexing(indexed: Expr, arg: Expr) extends Expr
  
  final case class ArrayInit(elemType: Type, size: Expr) extends Expr
  final case class StructInit(structName: String, args: List[Expr]) extends Expr
  
  final case class UnaryOp(operator: Operator, operand: Expr) extends Expr
  final case class BinaryOp(lhs: Expr, operator: Operator, rhs: Expr) extends Expr
  final case class Select(lhs: Expr, selected: String) extends Expr
  
  final case class VarAssig(lhs: Expr, rhs: Expr) extends Statement

  final case class IfThenElse(cond: Expr, thenBr: Statement, elseBrOpt: Option[Statement]) extends Statement
  final case class WhileLoop(cond: Expr, body: Statement) extends Statement
  final case class ReturnStat(value: Expr) extends Statement

}
