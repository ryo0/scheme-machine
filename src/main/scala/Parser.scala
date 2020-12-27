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
  def parseProgram(tokens: List[Token]): Program = {
    def parseProgramSub(tokens: List[Token], acm: List[Exp]): Program = {
      tokens match {
        case _ :: _ =>
          val (exp, rest) = parseExp(tokens)
          parseProgramSub(rest, acm :+ exp)
        case _ =>
          Program(acm)
      }
    }
    parseProgramSub(tokens, List())
  }
  def parseExp(tokens: List[Token]): (Exp, List[Token]) = {
    tokens match {
      case LParen :: Token.Symbol("quote") :: rest =>
        val (exp, rest2) = parseParen(rest, List())
        (QuoteExp(exp), rest2)
      case first :: rest =>
        first match {
          case LParen            => parseParen(rest, List())
          case Token.True        => (Bool(true), rest)
          case Token.False       => (Bool(false), rest)
          case Token.Str(str)    => (AST.Str(str), rest)
          case Token.Symbol(str) => (AST.Symbol(str), rest)
          case Token.Num(value)  => (AST.Num(value), rest)
          case Token.Quote =>
            val (q, rest2) = parseExp(rest)
            (AST.QuoteExp(q), rest2)
          case _ =>
            if (!first.isInstanceOf[Token.Op]) {
              throw new Error("error")
            }
            val op = opMap(first)
            (AST.Operator(op), rest)
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
