package compiler.parser

import compiler.Errors.ErrorReporter
import compiler.parser.ParseTree.^:
import compiler.parser.TreeParsers.*
import compiler.irs.Asts.{BinaryOp, Expr}
import compiler.irs.Tokens.OperatorToken
import lang.Operator

import scala.annotation.tailrec

object BinaryOperatorsParser {

  private def op(operators: List[Operator])(implicit errorReporter: ErrorReporter): FinalTreeParser[Operator] = {
    val descr = s"operator(s) ${operators.mkString(",")}"
    treeParser(descr){
      case OperatorToken(operator) if operators.contains(operator) => operator
    } setName descr
  }

  def buildFrom(operatorsByDecreasingPrecedence: List[List[Operator]], operand: AnyTreeParser[Expr])
               (implicit errorReporter: ErrorReporter): AnyTreeParser[Expr] = {

    @tailrec
    def recurse(currOperand: AnyTreeParser[Expr], remainingOps: List[List[Operator]], priorityLevel: Int)
               (implicit errorReporter: ErrorReporter): AnyTreeParser[Expr] = {
      remainingOps match {
        case Nil => currOperand
        case headOps :: tailOps =>
          val newParser = {
            currOperand ::: repeat(op(headOps) ::: currOperand) map {
              case leftOperand ^: Nil => leftOperand
              case leftOperand ^: opSeq =>
                opSeq.foldLeft(leftOperand){
                  (left, opAndRight) => BinaryOp(left, opAndRight.left, opAndRight.right)
                }
            }
          }
          newParser.setName(s"priority $priorityLevel expression")
          recurse(newParser, tailOps, priorityLevel-1)
      }
    }

    recurse(operand, operatorsByDecreasingPrecedence, priorityLevel = operatorsByDecreasingPrecedence.size)
  }

}
