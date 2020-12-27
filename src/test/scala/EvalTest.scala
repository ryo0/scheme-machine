import org.scalatest.FunSuite
import Lexer.tokenize
import AST._
import Data.Data
import Parser._
import eval._
class EvalTest extends FunSuite {
  def evaluate(str: String): Data = {
    val tokens = tokenize(str)
    val program = parseProgram(tokens)
    evalProgram(program)
  }
  test("eval op") {
    assert(evaluate("(+ 1 2 3)") === Data.Num(6))
    assert(evaluate("(* 1 2 3)") === Data.Num(6))
    assert(evaluate("(- 1 2 3)") === Data.Num(-4))
    assert(evaluate("(/ 12 2 3)") === Data.Num(2))
  }
  test("eval if") {
    assert(evaluate("(if #t 1 0)") === Data.Num(1))
    assert(evaluate("(if #f 1 0)") === Data.Num(0))
  }
  test("eval define") {
    assert(evaluate("(define x 1) (+ x x)") === Data.Num(2))
    assert(
      evaluate("(define x (+ 1 1)) (define y (+ x 2)) (* x y)") === Data.Num(8)
    )
  }
}
