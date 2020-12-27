import org.scalatest.FunSuite
import Lexer.tokenize
import AST._
import Data.Data
import Parser._
import eval._
class EvalTest extends FunSuite {
  def evaluate(str: String): Data = {
    val tokens = tokenize(str)
    val exp = parseExp(tokens)._1
    evalExp(exp)
  }
  test("eval op") {
    assert(evaluate("(+ 1 2 3)") === Data.Num(6))
    assert(evaluate("(* 1 2 3)") === Data.Num(6))
    assert(evaluate("(- 1 2 3)") === Data.Num(-4))
    assert(evaluate("(/ 12 2 3)") === Data.Num(2))
  }
}
