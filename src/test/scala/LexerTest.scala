import org.scalatest.FunSuite
import Lexer.tokenize
import Lexer.tokenizeSymbol
import Token.Symbol
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
  }
}
