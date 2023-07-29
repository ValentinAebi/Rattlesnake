package lang

enum Keyword(val str: String) {
  case Arr extends Keyword("arr")
  case As extends Keyword("as")
  case Else extends Keyword("else")
  case Fn extends Keyword("fn")
  case For extends Keyword("for")
  case If extends Keyword("if")
  case New extends Keyword("new")
  case Panic extends Keyword("panic")
  case Return extends Keyword("return")
  case Struct extends Keyword("struct")
  case Test extends Keyword("test")
  case Then extends Keyword("then")
  case Val extends Keyword("val")
  case Var extends Keyword("var")
  case When extends Keyword("when")
  case While extends Keyword("while")

  override def toString: String = str
}
