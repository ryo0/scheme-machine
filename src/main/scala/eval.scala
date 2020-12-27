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
  def castToNum(data: List[Data], acm: List[Data.Num]): List[Data.Num] = {
    data match {
      case first :: rest =>
        first match {
          case num: Data.Num =>
            castToNum(rest, acm :+ num)
          case _ =>
            throw new Error("cast 失敗、演算の引数が整数でない" + data)
        }
      case _ =>
        acm
    }
  }
  def evalOpExp(op: AST.Op, operands: List[Data]): Data = {
    val result = op match {
      case AST.Plus | AST.Minus | AST.Asterisk | AST.Slash | AST.Greater |
          AST.Less =>
        val operandsInt = castToNum(operands, List()).map(n => n.value)
        op match {
          case AST.Plus =>
            operandsInt.sum
          case AST.Minus =>
            operandsInt.tail.foldRight(operandsInt.head) { (a, b) =>
              b - a
            }
          case AST.Asterisk =>
            operandsInt.product
          case AST.Slash =>
            operandsInt.tail.foldRight(operandsInt.head) { (a, b) =>
              b / a
            }
        }
    }
    Data.Num(result)
  }
}
