object AST {
  case class Program(exps: List[Exp])
  sealed class Exp
  case class ParenExp(exps: List[Exp]) extends Exp
  sealed class Op
  case class Operator(op: Op) extends Exp
  object Plus extends Op
  object Minus extends Op
  object Asterisk extends Op
  object Slash extends Op
  object Equal extends Op
  object Greater extends Op
  object Less extends Op
  object True extends Exp
  object False extends Exp
  case class Symbol(str: String) extends Exp
  case class Str(str: String) extends Exp
  case class Num(value: Int) extends Exp
  case class Bool(b: Boolean) extends Exp
}
