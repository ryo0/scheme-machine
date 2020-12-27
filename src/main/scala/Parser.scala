import Token._
import AST._

import scala.annotation.tailrec

object Parser {
  val opMap: Map[Token, AST.Op] = Map(
    Token.Plus -> AST.Plus,
    Token.Minus -> AST.Minus,
    Token.Asterisk -> AST.Asterisk,
    Token.Slash -> AST.Slash,
    Token.Equal -> AST.Equal,
    Token.Greater -> AST.Greater,
    Token.Less -> AST.Less
  )
  def parseExp(tokens: List[Token]): (Exp, List[Token]) = {
    tokens match {
      case first :: rest =>
        first match {
          case LParen            => parseParen(rest, List())
          case Token.True        => (AST.True, rest)
          case Token.False       => (AST.False, rest)
          case Token.Str(str)    => (AST.Str(str), rest)
          case Token.Symbol(str) => (AST.Symbol(str), rest)
          case Token.Num(value)  => (AST.Num(value), rest)
          case _ =>
            if (!first.isInstanceOf[Token.Op]) {
              throw new Error("error")
            }
            val op = opMap(first)
            (op, rest)
        }
    }
  }
  def parse(tokens: List[Token]): Exp = {
    parseExp(tokens)._1
  }
  @tailrec
  def parseParen(
      tokens: List[Token],
      acm: List[Exp]
  ): (ParenExp, List[Token]) = {
    tokens match {
      case first :: rest =>
        first match {
          case RParen => (ParenExp(acm), rest)
          case _ =>
            val (exp, rest2) = parseExp(tokens)
            parseParen(rest2, acm :+ exp)
        }
      case _ =>
        (ParenExp(acm), List())
    }
  }
}
