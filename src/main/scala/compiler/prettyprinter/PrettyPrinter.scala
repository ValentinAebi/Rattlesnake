package compiler.prettyprinter

import compiler.CompilerStep
import compiler.irs.Asts.*
import lang.Keyword.*
import lang.Operator

final class PrettyPrinter(indentGranularity: Int = 2, displayAllParentheses: Boolean = false) extends CompilerStep[Ast, String] {

  override def apply(input: Ast): String = {
    val pps = new PrettyPrintString(indentGranularity)
    addAst(input)(pps)
    pps.built
  }

  private def addAst(ast: Ast)(implicit pps: PrettyPrintString): Unit = {
    ast match {

      case Source(defs) =>
        pps.newLine()
        val iter = defs.iterator
        while (iter.hasNext) {
          addAst(iter.next())
          if (iter.hasNext) {
            pps
              .add(";")
              .newLine()
              .newLine()
          }
        }
        pps.newLine()

      case Block(stats) =>
        addBracesList(stats, ";", onMultipleLines = true)

      case FunDef(funName, args, optRetType, body) =>
        pps
          .add(Fn.str)
          .addSpace()
          .add(funName)
        addParenthList(args)
        optRetType.foreach { retType =>
          pps
            .add(" -> ")
            .add(retType.toString)
            .addSpace()
        }
        addAst(body)

      case StructDef(structName, fields) =>
        pps
          .add(Struct.str)
          .addSpace()
          .add(structName)
          .addSpace()
        addBracesList(fields, ",", onMultipleLines = true)

      case Param(paramName, tpe) =>
        pps
          .add(paramName)
          .add(": ")
          .add(tpe.toString)

      case ValDef(valName, optType, rhs) =>
        pps
          .add(Val.str)
          .addSpace()
          .add(valName)
        optType.foreach { optT =>
          pps
            .add(": ")
            .add(optT.toString)
        }
        pps.add(" = ")
        addAst(rhs)

      case VarDef(varName, optType, rhs) =>
        pps
          .add(Var.str)
          .addSpace()
          .add(varName)
        optType.foreach { optT =>
          pps
            .add(": ")
            .add(optT.toString)
        }
        pps.add(" = ")
        addAst(rhs)

      case literal: Literal =>
        pps.add(literal.value.toString)

      case VariableRef(name) =>
        pps.add(name)

      case Call(callee, args) =>
        addAst(callee)
        addParenthList(args)

      case Indexing(indexed, arg) =>
        addAst(indexed)
        pps.add("[")
        addAst(arg)
        pps.add("]")

      case ArrayInit(elemType, size) =>
        pps
          .add(Arr.str)
          .addSpace()
          .add(elemType.toString)
          .add("(")
        addAst(size)
        pps.add(")")

      case StructInit(structName, args) =>
        pps
          .add(New.str)
          .addSpace()
          .add(structName)
          .addSpace()
        addBracesList(args, ", ", onMultipleLines = false)

      case UnaryOp(operator, operand) =>
        pps.add(operator.str)
        val displayParenth = operand.isInstanceOf[UnaryOp | BinaryOp]
        if (displayParenth) {
          pps.add("(")
        }
        addAst(operand)
        if (displayParenth) {
          pps.add(")")
        }

      case BinaryOp(lhs, operator, rhs) =>
        val displayParenthLeft = displayAllParentheses && rhs.isInstanceOf[UnaryOp | BinaryOp]
        if (displayParenthLeft) {
          pps.add("(")
        }
        addAst(lhs)
        if (displayParenthLeft) {
          pps.add(")")
        }
        pps
          .addSpace()
          .add(operator.str)
          .addSpace()
        val displayParenthRight =
          rhs match {
            case UnaryOp(_, _) => true
            case BinaryOp(_, subOp, _) =>
              displayAllParentheses || (Operator.priorities(subOp) < Operator.priorities(operator))
            case _ => false
          }
        if (displayParenthRight) {
          pps.add("(")
        }
        addAst(rhs)
        if (displayParenthRight) {
          pps.add(")")
        }

      case Select(lhs, selected) =>
        addAst(lhs)
        pps
          .add(".")
          .add(selected)

      case VarAssig(lhs, rhs) =>
        addAst(lhs)
        pps.add(" = ")
        addAst(rhs)

      case IfThenElse(cond, thenBr, elseBrOpt) =>
        pps
          .add(If.str)
          .addSpace()
        addAst(cond)
        pps.addSpace()
        addAst(thenBr)
        elseBrOpt.foreach { elseBr =>
          pps
            .addSpace()
            .add(Else.str)
            .addSpace()
          addAst(elseBr)
        }

      case WhileLoop(cond, body) =>
        pps
          .add(While.str)
          .addSpace()
        addAst(cond)
        pps.addSpace()
        addAst(body)

      case ReturnStat(value) =>
        pps
          .add(Return.str)
          .addSpace()
        addAst(value)
    }
  }

  private def addBracesList(ls: List[Ast], sep: String, onMultipleLines: Boolean)(implicit pps: PrettyPrintString): Unit = {
    val iter = ls.iterator
    if (onMultipleLines) {
      pps
        .startBlock()
        .newLine()
    } else {
      pps.add("{ ")
    }
    while (iter.hasNext) {
      addAst(iter.next())
      if (iter.hasNext) {
        pps
          .add(sep)
        if (onMultipleLines) {
          pps.newLine()
        }
      }
    }
    if (onMultipleLines) {
      pps.endBlock()
    }
    else {
      pps.add(" }")
    }
  }

  private def addParenthList(ls: List[Ast])(implicit pps: PrettyPrintString): Unit = {
    val iter = ls.iterator
    pps.add("(")
    while (iter.hasNext) {
      addAst(iter.next())
      if (iter.hasNext) {
        pps.add(", ")
      }
    }
    pps.add(")")
  }

}
