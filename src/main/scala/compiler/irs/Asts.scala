package compiler.irs

import compiler.Position
import lang.{FunctionSignature, Keyword, Operator}
import lang.Types.*
import lang.Types.PrimitiveType.*

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
  sealed abstract class Expr extends Statement {
    private var tpeOpt: Option[Type] = None
    
    def setTypeOpt(tpe: Option[Type]): Expr = {
      tpeOpt = tpe
      this
    }

    final def setType(tpe: Type): Expr = {
      setTypeOpt(Some(tpe))
    }
    
    def getTypeOpt: Option[Type] = tpeOpt
    
    final def getType: Type = {
      getTypeOpt match
        case Some(tpe) => tpe
        case None => throw new NoSuchElementException(s"type missing in $this")
    }
    
  }

  final case class Source(defs: List[TopLevelDef]) extends Ast {
    private var name: String = "<missing name>"

    def setName(name: String): Source = {
      this.name = name
      this
    }

    def getName: String = name
  }

  final case class Block(stats: List[Statement]) extends Statement

  sealed abstract class TopLevelDef extends Ast
  final case class FunDef(funName: String, params: List[Param], optRetType: Option[Type], body: Block) extends TopLevelDef {
    val signature: FunctionSignature = FunctionSignature(funName, params.map(_.tpe), optRetType.getOrElse(VoidType))
  }
  final case class StructDef(structName: String, fields: List[Param]) extends TopLevelDef
  final case class Param(paramName: String, tpe: Type) extends Ast

  final case class LocalDef(localName: String, var optType: Option[Type], rhs: Expr, isReassignable: Boolean) extends Statement {
    val keyword: Keyword = if isReassignable then Keyword.Var else Keyword.Val
  }

  sealed abstract class Literal extends Expr {
    val value: Any
  }
  final case class IntLit(value: Int) extends Literal {
    override def getTypeOpt: Option[Type] = Some(IntType)
  }
  final case class DoubleLit(value: Double) extends Literal {
    override def getTypeOpt: Option[Type] = Some(DoubleType)
  }
  final case class CharLit(value: Char) extends Literal {
    override def getTypeOpt: Option[Type] = Some(CharType)
  }
  final case class BoolLit(value: Boolean) extends Literal {
    override def getTypeOpt: Option[Type] = Some(BoolType)
  }
  final case class StringLit(value: String) extends Literal {
    override def getTypeOpt: Option[Type] = Some(StringType)
  }

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
  final case class Ternary(cond: Expr, thenBr: Expr, elseBr: Expr) extends Expr
  final case class WhileLoop(cond: Expr, body: Statement) extends Statement
  final case class ForLoop(
                            initStats: List[LocalDef | Assignment],
                            cond: Expr,
                            stepStats: List[Assignment],
                            body: Block
                          ) extends Statement
  final case class ReturnStat(value: Expr) extends Statement {
    def getRetType: Option[Type] = value.getTypeOpt
  }
  final case class PanicStat(msg: Expr) extends Statement

}
