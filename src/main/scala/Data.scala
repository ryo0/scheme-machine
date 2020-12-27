import AST.Program

object Data {
  sealed class Data
  case class Bool(b: Boolean) extends Data
  case class Op(op: AST.Op) extends Data
  case class Num(value: Int) extends Data
  case class Str(s: String) extends Data
  case class Symbol(v: String) extends Data
  case class DataList(lst: List[Data]) extends Data
  case class Procedure(p: List[Data] => Program) extends Data
  case class Quote(data: Data) extends Data
}
