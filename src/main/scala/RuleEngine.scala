package scala

import parser._

object RuleEngine extends App {

  class MyParser extends ExpressionParsers

  val parser = new MyParser

  val rule = "(row.f1 * 5) + 3"
    val parseResult = parser.parseAll(parser.expression, rule)

    println(parseResult)

    if (parseResult.successful) {
      val ev1 = new Evaluator(Row(1, 2, 3, "test"))
      val ev2 = new Evaluator(Row(2, 3, 4, "test"))
      val ev3 = new Evaluator(Row(3, 4, 5, "test"))
      println(parseResult.get.accept(ev1))
      println(parseResult.get.accept(ev2))
      println(parseResult.get.accept(ev3))
    }
}

class Evaluator(ad: Row) extends ExpressionVisitor {
  override def visit(e: Expression): Double = e match {
    case Constant(digit: Double) => digit

    case RowNumber(field: String) => field match {
      case "f1" => ad.feature1
      case "f2" => ad.feature2
      case "f3" => ad.feature3
    }

    case Add(expr1: Expression, expr2: Expression) => expr1.accept(this) + expr2.accept(this)

    case Sub(expr1: Expression, expr2: Expression) => expr1.accept(this) - expr2.accept(this)

    case Multiply(expr1: Expression, expr2: Expression) => expr1.accept(this) * expr2.accept(this)

    case Divide(expr1: Expression, expr2: Expression) => expr1.accept(this) / expr2.accept(this)

    case Minus(expr: Expression) => expr.accept(this)
  }
}

case class Row(feature1: Long, feature2: Long, feature3: Long, tag: String)
