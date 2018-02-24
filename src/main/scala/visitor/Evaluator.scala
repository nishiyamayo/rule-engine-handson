package visitor

import constant.Row
import parser._

class Evaluator(row: Row) extends ExpressionVisitor with WordVisitor {
  override def visit(e: Expression): Double = e match {
    case Constant(digit: String) => digit.toDouble

    case RowNumber(field: String) => row.getDouble(field)

    case Add(expr1: Expression, expr2: Expression) => expr1.accept(this) + expr2.accept(this)

    case Sub(expr1: Expression, expr2: Expression) => expr1.accept(this) - expr2.accept(this)

    case Multiply(expr1: Expression, expr2: Expression) => expr1.accept(this) * expr2.accept(this)

    case Divide(expr1: Expression, expr2: Expression) => expr1.accept(this) / expr2.accept(this)

    case Minus(expr: Expression) => expr.accept(this)
  }

  override def visit(w: Word): String = w match {
    case RowString(field: String) => row.getString(field)

    case ConstantWord(word: String) => word
  }
}
