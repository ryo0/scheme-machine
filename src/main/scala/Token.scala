object Token {
  sealed class Token

  case class Num(value: Float) extends Token
  case class Str(value: String) extends Token
  case class Symbol(value: String) extends Token
  object True extends Token
  object False extends Token
  object LParen extends Token
  object RParen extends Token

  sealed class Op extends Token
  object Plus extends Op
  object Minus extends Op
  object Asterisk extends Op
  object Slash extends Op
}
