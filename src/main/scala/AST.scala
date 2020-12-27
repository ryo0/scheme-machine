object AST {
  sealed class Exp
  case class ParenExp(exps: List[Exp]) extends Exp
  sealed class Op extends Exp
  object Plus extends Op
  object Minus extends Op
  object Asterisk extends Op
  object Slash extends Op
  object Equal extends Op
  object Greater extends Op
  object Less extends Op

  case class Symbol(str: String) extends Exp
  case class String(str: String) extends Exp
  case class Bool(b: Boolean) extends Exp
}
