import AST._
import Parser._
import Data.Data
import Token._

object eval {
  def evalExp(exp: Exp): Data = {
    exp match {
      case op: AST.Op =>
        Data.Op(op)
      case AST.Num(value) =>
        Data.Num(value)
      case ParenExp(exps) => evalParenExp(ParenExp(exps))
    }
  }
  def evalParenExp(exp: ParenExp): Data = {
    exp.exps.map(e => evalExp(e)) match {
      case first :: rest =>
        first match {
          case Data.Op(op) =>
            evalOpExp(op, rest)
        }
    }
  }
  def evalOpExp(op: AST.Op, operands: List[Data]): Data = {
    val result = op match {
      case AST.Plus =>
        operands.map(d => d.asInstanceOf[Data.Num].value).sum
      case AST.Minus =>
        val operandInts = operands.map(d => d.asInstanceOf[Data.Num].value)
        operandInts.tail.foldRight(operandInts.head) { (a, b) =>
          b - a
        }
      case AST.Asterisk =>
        operands.map(d => d.asInstanceOf[Data.Num].value).product
      case AST.Slash =>
        val operandInts = operands.map(d => d.asInstanceOf[Data.Num].value)
        operandInts.tail.foldRight(operandInts.head) { (a, b) =>
          b / a
        }
    }
    Data.Num(result)
  }
}
