package parser

trait ExpressionVisitor {
  def visit(e: Expression): Double
}

trait Expression {
  def accept(visitor: ExpressionVisitor): Double = {
    visitor.visit(this)
  }
}

case class Constant(digit: String) extends Expression

case class RowNumber(field: String) extends Expression

case class Add(expr1: Expression, expr2: Expression) extends Expression

case class Sub(expr1: Expression, expr2: Expression) extends Expression

case class Multiply(expr1: Expression, expr2: Expression) extends Expression

case class Divide(expr1: Expression, expr2: Expression) extends Expression

case class Minus(expr: Expression) extends Expression

trait ExpressionParsers extends MyParsers {

  def expression: Parser[Expression] = term ~ rep(("+" | "-") ~ term) ^^ {
    case f ~ fs => fs.foldLeft(f) {
      case (acc, c) => Add(acc, c._2)
      case (acc, c) => Sub(acc, c._2)
    }
  }

  def term: Parser[Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case f ~ fs => fs.foldLeft(f) {
      case (acc, c) => Multiply(acc, c._2)
      case (acc, c) => Divide(acc, c._2)
    }
  }

  def factor: Parser[Expression] = unary

  def unary: Parser[Expression] = ("+" | "-") ~ eprimary ^^ {
    case "+" ~ u => u
    case "-" ~ u => Minus(u)
  } | eprimary

  def eprimary: Parser[Expression] = "(" ~ expression ~ ")" ^^ {
    case _ ~ expr ~ _ => expr
  } | value

  def value: Parser[Expression] = (variable | number) ^^ {
    case variable => variable
    case number => number
  }

  def variable: Parser[Expression] = reVariable ^^ {
    case _ ~ _ ~ field => RowNumber(field)
  }

  def number: Parser[Expression] = decimalNumber ^^ {
    v => Constant(v)
  }
}
