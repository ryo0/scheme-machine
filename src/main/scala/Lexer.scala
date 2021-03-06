import Token.Token
import Token._

import scala.annotation.tailrec

object Lexer {
  val symbolToTokenMap: Map[Char, Token] =
    Map(
      '(' -> LParen,
      ')' -> RParen,
      '+' -> Plus,
      '-' -> Minus,
      '*' -> Asterisk,
      '/' -> Slash,
      '\'' -> Quote,
      '=' -> Equal,
      '>' -> Greater,
      '<' -> Less
    )
  def skip(c: Char): Boolean = {
    if (c.isSpaceChar || c == '\n') {
      true
    } else {
      false
    }
  }
  val symbolChar = List('-', '!', '?')
  def isSymbolChar(c: Char): Boolean = {
    c.isLetter || c.isDigit || symbolChar.contains(c)
  }
  def tokenize(str: String): List[Token] = {
    @tailrec
    def tokenizeSub(str: List[Char], acm: List[Token]): (List[Token]) = {
      str match {
        case first :: rest => {
          if (skip(first)) {
            tokenizeSub(rest, acm)
          } else if (first.isLetter) {
            val (sym, rest2) = tokenizeSymbol(str)
            tokenizeSub(rest2, acm :+ sym)
          } else if (first.isDigit) {
            val (num, rest2) = tokenizeNum(str)
            tokenizeSub(rest2, acm :+ num)
          } else if (first == '"') {
            val (strToken, rest2) = tokenizeStr(rest)
            tokenizeSub(rest2, acm :+ strToken)
          } else if (symbolToTokenMap.contains(first)) {
            val token = symbolToTokenMap(first)
            tokenizeSub(rest, acm :+ token)
          } else if (first == '#') {
            if (rest.nonEmpty) {
              val second = rest.head
              val b = if (second == 't') {
                True
              } else if (second == 'f') {
                False
              } else {
                throw new Error("error" + second)
              }
              tokenizeSub(rest.tail, acm :+ b)
            } else {
              throw new Error("error" + first)
            }
          } else {
            throw new Error(
              "panic, first: " + first + "rest: " + rest + "acm: " + acm
            )
          }
        }
        case _ =>
          acm
      }
    }
    tokenizeSub(str.toList, List())
  }

  def tokenizeStr(str: List[Char]): (Str, List[Char]) = {
    @tailrec
    def tokenizeStrSub(str: List[Char], acm: List[Char]): (Str, List[Char]) = {
      str match {
        case first :: rest =>
          if (first == '"') {
            (Str(acm.mkString("")), rest)
          } else {
            tokenizeStrSub(rest, acm :+ first)
          }
        case _ => throw new Error("error" + str)
      }
    }
    tokenizeStrSub(str, List())
  }

  def tokenizeNum(str: List[Char]): (Num, List[Char]) = {
    @tailrec
    def tokenizeNumSub(
        str: List[Char],
        acm: List[Char]
    ): (Num, List[Char]) = {
      str match {
        case first :: rest =>
          if (skip(first)) {
            (Num(acm.mkString("").toInt), rest)
          } else if (first.isDigit) {
            tokenizeNumSub(rest, acm :+ first)
          } else {
            (Num(acm.mkString("").toInt), str)
          }
        case _ =>
          (Num(acm.mkString("").toInt), str)
      }
    }
    tokenizeNumSub(str, List())
  }

  def tokenizeSymbol(str: List[Char]): (Symbol, List[Char]) = {
    @tailrec
    def tokenizeSymbolSub(
        str: List[Char],
        acm: List[Char]
    ): (Symbol, List[Char]) = {
      str match {
        case first :: rest =>
          if (skip(first)) {
            (Symbol(acm.mkString("")), rest)
          } else if (isSymbolChar(first)) {
            tokenizeSymbolSub(rest, acm :+ first)
          } else {
            (Symbol(acm.mkString("")), str)
          }
        case _ =>
          (Symbol(acm.mkString("")), str)
      }
    }
    tokenizeSymbolSub(str, List())
  }
}
