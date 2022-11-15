package compiler.desugarer

import compiler.irs.Asts.{Ast, Source}
import compiler.{AnalysisContext, CompilerStep}
import compiler.irs.Asts.*
import lang.Operator.*
import lang.Operators
import lang.Types.PrimitiveType.{IntType, DoubleType, BoolType}

/**
 * Desugaring replaces:
 *  - `>`, `>=` ---> reversed
 *  - `x <= y` ---> `(x < y) || (x == y)`
 *  - `x != y` ---> `!(x == y)`
 *  - `VarModif`: `x += y` ---> `x = x + y`
 *  - `for` ---> `while`
 *  - `-x` ---> `0 - x`
 *  - `!x` ---> `when x then false else true`
 *  - `x && y` ---> `when x then y else false`
 *  - `x || y` ---> `when x then true else y`
 */
final class Desugarer extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    val (sources, ctx) = input
    val desugaredSources = sources.map(desugar)
    desugaredSources.foreach(_.assertAllTypesAreSet())
    (desugaredSources, ctx)
  }

  private def desugar(src: Source): Source = Source(src.defs.map(desugar)).setName(src.getName)

  private def desugar(block: Block): Block = Block(block.stats.map(desugar))

  private def desugar(funDef: FunDef): FunDef = {
    FunDef(funDef.funName, funDef.params.map(desugar), funDef.optRetType, desugar(funDef.body))
  }

  private def desugar(structDef: StructDef): StructDef = {
    StructDef(structDef.structName, structDef.fields.map(desugar))
  }

  private def desugar(param: Param): Param = param

  private def desugar(localDef: LocalDef): LocalDef =
    LocalDef(localDef.localName, localDef.optType, desugar(localDef.rhs), localDef.isReassignable)

  private def desugar(varAssig: VarAssig): VarAssig = VarAssig(desugar(varAssig.lhs), desugar(varAssig.rhs))

  private def desugar(varModif: VarModif): VarAssig = {
    val VarModif(lhs, rhs, op) = varModif
    val desugaredLhs = desugar(lhs)
    val desugaredRhs = desugar(rhs)
    VarAssig(desugaredLhs, BinaryOp(desugaredLhs, op, desugaredRhs).setType(lhs.getType))
  }

  private def desugar(ifThenElse: IfThenElse): IfThenElse = {
    IfThenElse(desugar(ifThenElse.cond), desugar(ifThenElse.thenBr), ifThenElse.elseBrOpt.map(desugar))
  }

  private def desugar(whileLoop: WhileLoop): WhileLoop = {
    WhileLoop(desugar(whileLoop.cond), desugar(whileLoop.body))
  }

  private def desugar(forLoop: ForLoop): Block = {
    val body = Block(
      forLoop.body.stats ++ forLoop.stepStats
    )
    val stats: List[Statement] = forLoop.initStats :+ WhileLoop(forLoop.cond, body)
    desugar(Block(stats))
  }

  private def desugar(returnStat: ReturnStat): ReturnStat = ReturnStat(returnStat.optVal.map(desugar))

  private def desugar(panicStat: PanicStat): PanicStat = PanicStat(desugar(panicStat.msg))

  private def desugar(expr: Expr): Expr = {
    val desugared = expr match {
      case literal: Literal => literal
      case varRef: VariableRef => varRef
      case call: Call => Call(desugar(call.callee), call.args.map(desugar))
      case indexing: Indexing => Indexing(desugar(indexing.indexed), desugar(indexing.arg))
      case arrayInit: ArrayInit => ArrayInit(arrayInit.elemType, desugar(arrayInit.size))
      case structInit: StructInit => StructInit(structInit.structName, structInit.args.map(desugar))
      case UnaryOp(operator, operand) =>
        val desugaredOperand = desugar(operand)
        operator match {
          case Minus if operand.getType == IntType => BinaryOp(IntLit(0), Minus, desugaredOperand)
          case Minus if operand.getType == DoubleType => BinaryOp(DoubleLit(0.0), Minus, desugaredOperand)
          case ExclamationMark => Ternary(desugaredOperand, BoolLit(false), BoolLit(true))
          case _ => UnaryOp(operator, desugaredOperand)
        }
      case binaryOp: BinaryOp => {
        val desugaredLhs = desugar(binaryOp.lhs)
        val desugaredRhs = desugar(binaryOp.rhs)
        binaryOp.operator match {
          case LessOrEq => desugar(BinaryOp(
            BinaryOp(desugaredLhs, LessThan, desugaredRhs).setType(BoolType),
            Or,
            BinaryOp(desugaredLhs, Equality, desugaredRhs).setType(BoolType)
          ))
          case GreaterThan => desugar(BinaryOp(desugaredRhs, LessThan, desugaredLhs))
          case GreaterOrEq => desugar(BinaryOp(desugaredRhs, LessOrEq, desugaredLhs))
          case Inequality =>
            desugar(UnaryOp(ExclamationMark,
              BinaryOp(desugaredLhs, Equality, desugaredRhs).setType(BoolType)
            ).setType(BoolType))
          case And => desugar(Ternary(desugaredLhs, desugaredRhs, BoolLit(false)))
          case Or => desugar(Ternary(desugaredLhs, BoolLit(true), desugaredRhs))
          case _ => BinaryOp(desugaredLhs, binaryOp.operator, desugaredRhs)
        }
      }
      case select: Select => Select(desugar(select.lhs), select.selected)
      case Ternary(cond, thenBr, elseBr) => Ternary(desugar(cond), desugar(thenBr), desugar(elseBr))
    }
    desugared.setTypeOpt(expr.getTypeOpt)
  }

  private def desugar(statement: Statement): Statement = {
    statement match
      case expr: Expr => desugar(expr)
      case block: Block => desugar(block)
      case localDef: LocalDef => desugar(localDef)
      case varAssig: VarAssig => desugar(varAssig)
      case varModif: VarModif => desugar(varModif)
      case ifThenElse: IfThenElse => desugar(ifThenElse)
      case whileLoop: WhileLoop => desugar(whileLoop)
      case forLoop: ForLoop => desugar(forLoop)
      case returnStat: ReturnStat => desugar(returnStat)
      case panicStat: PanicStat => desugar(panicStat)
  }

  private def desugar(topLevelDef: TopLevelDef): TopLevelDef = {
    topLevelDef match
      case funDef: FunDef => desugar(funDef)
      case structDef: StructDef => desugar(structDef)
  }

}
