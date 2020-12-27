import org.scalatest.FunSuite
import Lexer.tokenize
import AST._
import Parser._
class ParseTest extends FunSuite {
  def strToExp(str: String): Exp = {
    val tokens = tokenize(str)
    val exp = parseExp(tokens)._1
    exp
  }
  test("parse") {
    assert(
      strToExp("(define x (+ 1 1))") ===
        ParenExp(
          List(
            Symbol("define"),
            Symbol("x"),
            ParenExp(List(Operator(Plus), Num(1), Num(1)))
          )
        )
    )
    assert(
      strToExp("(define (len lst) (if (null? lst) 0 (+ 1 (len (cdr lst)))") ===
        ParenExp(
          List(
            Symbol("define"),
            ParenExp(List(Symbol("len"), Symbol("lst"))),
            ParenExp(
              List(
                Symbol("if"),
                ParenExp(List(Symbol("null?"), Symbol("lst"))),
                Num(0),
                ParenExp(
                  List(
                    Operator(Plus),
                    Num(1),
                    ParenExp(
                      List(
                        Symbol("len"),
                        ParenExp(List(Symbol("cdr"), Symbol("lst")))
                      )
                    )
                  )
                )
              )
            )
          )
        )
    )
  }
}
