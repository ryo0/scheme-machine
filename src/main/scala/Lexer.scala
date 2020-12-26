import Token.{Num, Symbol, Token}

import scala.annotation.tailrec

object Lexer {
  def skip(c: Char): Boolean = {
    if (c.isSpaceChar || c == '\n') {
      true
    } else {
      false
    }
  }
  def tokenize(str: String): List[Token] = {
    def tokenizeSub(str: List[Char], acm: List[Token]): (List[Token]) = {
      str match {
        case first :: rest => {
          if (skip(first)) {
            tokenizeSub(rest, acm)
          }
          if (first.isLetter) {
            val (sym, rest2) = tokenizeSymbol(str)
            tokenizeSub(rest2, acm :+ sym)
          } else if (first.isDigit) {
            val (num, rest2) = tokenizeNum(str)
            tokenizeSub(rest2, acm :+ num)
          } else {
            throw new Error(
              "panic, first: " + first + "rest: " + rest + "acm: " + acm
            )
          }
        }
        case _ => {
          acm
        }
      }
    }
    tokenizeSub(str.toList, List())
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
          } else if (first.isLetter) {
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
