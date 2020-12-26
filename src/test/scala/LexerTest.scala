import org.scalatest.FunSuite
import Lexer.tokenize
import Lexer.tokenizeSymbol
import Token.{Num, Symbol}
class LexerTest extends FunSuite {
  test("lexer") {
    assert(
      tokenizeSymbol("let ".toList)._1 ===
        Symbol("let")
    )
    assert(
      tokenize("let x eq") ===
        List(Symbol("let"), Symbol("x"), Symbol("eq"))
    )

    assert(
      tokenize("let x eq 100") ===
        List(Symbol("let"), Symbol("x"), Symbol("eq"), Num(100))
    )
  }
}
