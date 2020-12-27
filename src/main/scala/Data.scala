import AST.{Exp, Program}

object Data {
  sealed class Data
  object Null extends Data
  case class Bool(b: Boolean) extends Data
  case class Operator(op: AST.Op) extends Data
  case class Num(value: Int) extends Data
  case class Str(s: String) extends Data
  case class Symbol(v: String) extends Data
  case class DataList(lst: List[Data]) extends Data
  case class Procedure(p: List[Data] => Data) extends Data
  case class Quote(exp: Exp) extends Data

}
