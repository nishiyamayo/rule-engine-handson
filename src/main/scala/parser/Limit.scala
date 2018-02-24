package parser

trait LimitVisitor {
  def visit(limit: Limit): Int
}

trait Limit {
  def accept(visitor: LimitVisitor): Int = {
    visitor.visit(this)
  }
}

case class AggregationLimit(expr: Aggregation) extends Limit

class LimitParsers extends AggregationParsers {
  def limit: Parser[Limit] = "LIMIT" ~ aExpression ^^ {
    case _ ~ expr => AggregationLimit(expr)
  }
}
