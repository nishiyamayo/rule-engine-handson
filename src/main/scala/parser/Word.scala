package parser

trait WordVisitor {
  def visit(e: Word): String
}

trait Word {
  def accept(visitor: WordVisitor): String = {
    visitor.visit(this)
  }
}

case class RowString(field: String) extends Word

case class ConstantWord(word: String) extends Word

trait WordParsers extends MyParsers {
  def word: Parser[Word] = "word:" ~ reVariable ^^ {
    case _ ~ (_ ~ _ ~ field) => RowString(field)
  } | "word:" ~  reWord ^^ {
    case _ ~ w => ConstantWord(w)
  }
}
