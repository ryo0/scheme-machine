import AST._
import Data.Data

import scala.annotation.tailrec

object eval {
  type Env = Map[String, Data]
  def evalExp(exp: Exp): Data = {
    exp match {
      case AST.Bool(b) => Data.Bool(b)
      case AST.Num(value) =>
        Data.Num(value)
      case ParenExp(exps) => evalParenExp(ParenExp(exps))
    }
  }
  def evalParenExp(exp: ParenExp): Data = {
    exp.exps match {
      case first :: rest =>
        first match {
          case AST.Symbol(str) =>
            str match {
              case "if"    => evalIf(exp)
              case "true"  => Data.Bool(true)
              case "false" => Data.Bool(false)
            }
          case AST.Operator(op) =>
            evalOpExp(op, rest)
        }
    }
  }
  def car[T](lst: List[T]): T = {
    lst.head
  }
  def cdr[T](lst: List[T]): List[T] = {
    lst.tail
  }
  def cadr[T](lst: List[T]): T = {
    car(cdr(lst))
  }
  def cddr[T](lst: List[T]): List[T] = {
    cdr(cdr(lst))
  }
  def caddr[T](lst: List[T]): T = {
    car(cdr(cdr(lst)))
  }
  def cdddr[T](lst: List[T]): List[T] = {
    cdr(cdr(cdr(lst)))
  }
  def cadddr[T](lst: List[T]): T = {
    car(cdr(cdr(cdr(lst))))
  }

  def evalIf(parenExp: ParenExp): Data = {
    val exps = parenExp.exps
    val cond = evalExp(exps.tail.head)
    cond match {
      case Data.Bool(b) =>
        if (b) {
          evalExp(caddr(exps))
        } else {
          evalExp(cadddr(exps))
        }
    }
  }
  @tailrec
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
  def evalOpExp(op: AST.Op, operands: List[Exp]): Data = {
    val result = op match {
      case AST.Plus | AST.Minus | AST.Asterisk | AST.Slash | AST.Greater |
          AST.Less =>
        val operandsInt =
          castToNum(operands.map(o => evalExp(o)), List()).map(n => n.value)
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
