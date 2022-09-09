package compiler.irs

import compiler.Position
import lang.Operator
import lang.Types.*

object Asts {

  sealed abstract class Ast {
    // Positions are propagated by the TreeParser
    // Each AST is assigned the position of its leftmost token (by the map method of TreeParser)
    private var positionOpt: Option[Position] = None

    final def setPosition(posOpt: Option[Position]): Unit = {
      positionOpt = posOpt
    }
    
    final def setPosition(pos: Position): Unit = {
      setPosition(Some(pos))
    }

    final def getPosition: Option[Position] = positionOpt
  }

  sealed abstract class Statement extends Ast
  sealed abstract class Expr extends Statement

  final case class Source(defs: List[TopLevelDef]) extends Ast
  final case class Block(stats: List[Statement]) extends Statement

  sealed abstract class TopLevelDef extends Ast
  final case class FunDef(funName: String, params: List[Param], optRetType: Option[Type], body: Block) extends TopLevelDef
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
  
  sealed abstract class Assignment extends Statement
  
  final case class VarAssig(lhs: Expr, rhs: Expr) extends Assignment

  final case class VarModif(lhs: Expr, rhs: Expr, op: Operator) extends Assignment

  final case class IfThenElse(cond: Expr, thenBr: Statement, elseBrOpt: Option[Statement]) extends Statement
  final case class WhileLoop(cond: Expr, body: Statement) extends Statement
  final case class ForLoop(
                            initStats: List[ValDef | VarDef | Assignment],
                            cond: Expr,
                            stepStats: List[Assignment],
                            body: Block
                          ) extends Statement
  final case class ReturnStat(value: Expr) extends Statement {
    private var retType: Option[Type] = None
    def setRetType(tpe: Type): Unit = {
      retType = Some(tpe)
    }

    def getRetType: Option[Type] = retType
  }
  final case class PanicStat(msg: Expr) extends Statement
  
  def collect[T](ast: Ast)(pf: PartialFunction[Ast, T]): List[T] = {
    
    def recurse(ast: Ast): List[T] = collect(ast)(pf)
    
    val t = pf.lift.apply(ast)
    val recursive = ast match {
      case Source(defs) =>
        defs.flatMap(recurse)
      case Block(stats) =>
        stats.flatMap(recurse)
      case FunDef(_, params, _, body) =>
        params.flatMap(recurse) ++ recurse(body)
      case StructDef(_, fields) =>
        fields.flatMap(recurse)
      case Param(_, _) => Nil
      case ValDef(_, _, rhs) =>
        recurse(rhs)
      case VarDef(_, _, rhs) =>
        recurse(rhs)
      case _: Literal => Nil
      case VariableRef(_) => Nil
      case Call(callee, args) =>
        recurse(callee) ++ args.flatMap(recurse)
      case Indexing(indexed, arg) =>
        recurse(indexed) ++ recurse(arg)
      case ArrayInit(_, size) =>
        recurse(size)
      case StructInit(_, args) =>
        args.flatMap(recurse)
      case UnaryOp(_, operand) =>
        recurse(operand)
      case BinaryOp(lhs, _, rhs) =>
        recurse(lhs) ++ recurse(rhs)
      case Select(lhs, _) =>
        recurse(lhs)
      case VarAssig(lhs, rhs) =>
        recurse(lhs) ++ recurse(rhs)
      case VarModif(lhs, rhs, _) =>
        recurse(lhs) ++ recurse(rhs)
      case IfThenElse(cond, thenBr, elseBrOpt) =>
        recurse(cond) ++ recurse(thenBr) ++ elseBrOpt.map(recurse).getOrElse(Nil)
      case WhileLoop(cond, body) =>
        recurse(cond) ++ recurse(body)
      case ForLoop(initStats, cond, stepStats, body) =>
        initStats.flatMap(recurse) ++ recurse(cond) ++ stepStats.flatMap(recurse) ++ recurse(body)
      case ReturnStat(value) =>
        recurse(value)
      case PanicStat(msg) =>
        recurse(msg)
    }
    t.toList ++ recursive
  }

}
