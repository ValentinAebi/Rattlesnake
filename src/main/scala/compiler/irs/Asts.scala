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

    def children: List[Ast]

    final def collect[T](f: Ast => T): List[T] = {
      f(this) :: children.flatMap(_.collect(f))
    }

    final def assertAllTypesAreSet(): Unit = {
      collect {
        case expr: Expr => assert(expr.getTypeOpt.isDefined, s"no type set for $expr")
        case _ => ()
      }
    }
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

    override def children: List[Ast] = defs

    def setName(name: String): Source = {
      this.name = name
      this
    }

    def getName: String = name
  }

  final case class Block(stats: List[Statement]) extends Statement {
    override def children: List[Ast] = stats
  }

  sealed abstract class TopLevelDef extends Ast
  final case class FunDef(funName: String, params: List[Param], optRetType: Option[Type], body: Block) extends TopLevelDef {
    val signature: FunctionSignature = FunctionSignature(funName, params.map(_.tpe), optRetType.getOrElse(VoidType))

    override def children: List[Ast] = params :+ body
  }
  final case class StructDef(structName: String, fields: List[Param]) extends TopLevelDef {
    override def children: List[Ast] = fields
  }
  final case class Param(paramName: String, tpe: Type) extends Ast {
    override def children: List[Ast] = Nil
  }

  final case class LocalDef(localName: String, var optType: Option[Type], rhs: Expr, isReassignable: Boolean) extends Statement {
    val keyword: Keyword = if isReassignable then Keyword.Var else Keyword.Val

    override def children: List[Ast] = List(rhs)
  }

  sealed abstract class Literal extends Expr {
    val value: Any

    final override def children: List[Ast] = Nil
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

  final case class VariableRef(name: String) extends Expr {
    override def children: List[Ast] = Nil
  }
  final case class Call(callee: Expr, args: List[Expr]) extends Expr {
    override def children: List[Ast] = callee :: args
  }
  final case class Indexing(indexed: Expr, arg: Expr) extends Expr {
    override def children: List[Ast] = List(indexed, arg)
  }
  
  final case class ArrayInit(elemType: Type, size: Expr) extends Expr {
    override def children: List[Ast] = List(size)
  }

  final case class FilledArrayInit(arrayElems: List[Expr]) extends Expr {
    override def children: List[Ast] = arrayElems
  }
  final case class StructInit(structName: String, args: List[Expr]) extends Expr {
    override def children: List[Ast] = args
  }
  
  final case class UnaryOp(operator: Operator, operand: Expr) extends Expr {
    override def children: List[Ast] = List(operand)
  }
  final case class BinaryOp(lhs: Expr, operator: Operator, rhs: Expr) extends Expr {
    override def children: List[Ast] = List(lhs, rhs)
  }
  final case class Select(lhs: Expr, selected: String) extends Expr {
    override def children: List[Ast] = List(lhs)
  }
  
  sealed abstract class Assignment extends Statement
  
  final case class VarAssig(lhs: Expr, rhs: Expr) extends Assignment {
    override def children: List[Ast] = List(lhs, rhs)
  }

  final case class VarModif(lhs: Expr, rhs: Expr, op: Operator) extends Assignment {
    override def children: List[Ast] = List(lhs, rhs)
  }

  final case class IfThenElse(cond: Expr, thenBr: Statement, elseBrOpt: Option[Statement]) extends Statement {
    override def children: List[Ast] = List(cond, thenBr) ++ elseBrOpt
  }
  final case class Ternary(cond: Expr, thenBr: Expr, elseBr: Expr) extends Expr {
    override def children: List[Ast] = List(cond, thenBr, elseBr)
  }
  final case class WhileLoop(cond: Expr, body: Statement) extends Statement {
    override def children: List[Ast] = List(cond, body)
  }
  final case class ForLoop(
                            initStats: List[LocalDef | Assignment],
                            cond: Expr,
                            stepStats: List[Assignment],
                            body: Block
                          ) extends Statement {
    override def children: List[Ast] = initStats ++ List(cond) ++ stepStats :+ body
  }
  final case class ReturnStat(optVal: Option[Expr]) extends Statement {
    def getRetType: Option[Type] = if optVal.isDefined then optVal.get.getTypeOpt else Some(VoidType)

    override def children: List[Ast] = optVal.toList
  }
  final case class PanicStat(msg: Expr) extends Statement {
    override def children: List[Ast] = List(msg)
  }
  
  final case class Sequence(stats: List[Statement], expr: Expr) extends Expr {
    override def children: List[Ast] = stats :+ expr

    override def getTypeOpt: Option[Type] = expr.getTypeOpt
  }

}
