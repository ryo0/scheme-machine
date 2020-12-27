import AST._
import Data.Data

import scala.annotation.tailrec
import scala.collection.mutable

object eval {
  type Env = mutable.Map[String, Data]
  val map: Env = mutable.Map()
  def getValFromEnv(key: String, env: Env): Data = {
    val value = env.get(key)
    value match {
      case Some(v) => v
      case None    => throw new Error("error env: " + env)
    }
  }
  def setValToEnv(key: String, value: Data, env: Env): Unit = {
    env(key) = value
  }
  def extendEnv(params: List[Data.Symbol], args: List[Data], env: Env): Env = {
    assert(params.length == args.length)
    val len = params.length
    for (i <- 0 until len) {
      setValToEnv(params(i).v, args(i), env)
    }
    env
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

  def eval(program: Program): Data = {
    evalProgram(program, mutable.Map(): Env)
  }

  def evalProgram(program: Program, env: Env): Data = {
    program.exps
      .map(e => {
        evalExp(e, env)
      })
      .last
  }

  def evalExp(exp: Exp, env: Env): Data = {
    exp match {
      case AST.Bool(b) => Data.Bool(b)
      case AST.Num(value) =>
        Data.Num(value)
      case ParenExp(exps) => evalParenExp(ParenExp(exps), env)
      case Symbol(str)    => getValFromEnv(str, env)
    }
  }
  def evalParenExp(exp: ParenExp, env: Env): Data = {
    exp.exps match {
      case first :: rest =>
        first match {
          case AST.Symbol(str) =>
            str match {
              case "if"     => evalIf(exp, env)
              case "true"   => Data.Bool(true)
              case "false"  => Data.Bool(false)
              case "define" => evalDefine(exp, env)
              case "lambda" => evalLambda(exp, env)
              case _ =>
                val p = evalExp(first, env).asInstanceOf[Data.Procedure]
                println(p, rest)
                apply(p, rest, env)
            }
          case AST.Operator(op) =>
            evalOpExp(op, rest, env)
          case _ =>
            val p = evalExp(first, env).asInstanceOf[Data.Procedure]
            apply(p, rest, env)
        }
    }
  }

  def apply(procedure: Data.Procedure, args: List[Exp], env: Env): Data = {
    val evaledArgs = args.map(a => evalExp(a, env))
    procedure.p(evaledArgs)
  }

  def evalLambda(exp: AST.ParenExp, env: Env): Data.Procedure = {
    // (lambda (x) (+ x 1))
    val exps = exp.exps
    val params = cadr(exps)
    val body = cddr(exps)
    val paramExps = params.asInstanceOf[ParenExp].exps
    val paramSymbols =
      paramExps.map(e => Data.Symbol(e.asInstanceOf[Symbol].str))
    Data.Procedure(p = args => {
      val newEnv = extendEnv(paramSymbols, args, env)
      evalProgram(Program(body), newEnv)
    })
  }

  def evalDefine(exp: ParenExp, env: Env): Data = {
    // (define x 1)
    val exps = exp.exps
    val name = cadr(exps).asInstanceOf[Symbol].str
    val value = evalExp(caddr(exps), env)
    setValToEnv(name, value, env)
    Data.Null
  }

  def evalIf(parenExp: ParenExp, env: Env): Data = {
    val exps = parenExp.exps
    val cond = evalExp(exps.tail.head, env)
    cond match {
      case Data.Bool(b) =>
        if (b) {
          evalExp(caddr(exps), env)
        } else {
          evalExp(cadddr(exps), env)
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
  def evalOpExp(op: AST.Op, operands: List[Exp], env: Env): Data = {
    val result = op match {
      case AST.Plus | AST.Minus | AST.Asterisk | AST.Slash | AST.Greater |
          AST.Less =>
        val operandsInt =
          castToNum(operands.map(o => evalExp(o, env)), List()).map(n =>
            n.value
          )
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
