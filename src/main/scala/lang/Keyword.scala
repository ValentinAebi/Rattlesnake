package lang

enum Keyword(val str: String) {
  case Val extends Keyword("val")
  case Var extends Keyword("var")
  case If extends Keyword("if")
  case Else extends Keyword("else")
  case While extends Keyword("while")
  case For extends Keyword("for")
  case Fn extends Keyword("fn")
  case Return extends Keyword("return")
  case Struct extends Keyword("struct")
  case Arr extends Keyword("arr")
  case New extends Keyword("new")
  case Panic extends Keyword("panic")

  override def toString: String = str
}
