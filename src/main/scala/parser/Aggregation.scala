package parser

trait AggregationVisitor {
  def visit(e: Aggregation): Double
}

trait Aggregation {
  def accept(visitor: AggregationVisitor): Double = {
    visitor.visit(this)
  }
}

case class SumAggregation(expression: Expression) extends Aggregation

case class AveAggregation(expression: Expression) extends Aggregation

case class CountAggregation(expression: Expression) extends Aggregation

case class MaxAggregation(expression: Expression) extends Aggregation

case class MinAggregation(expression: Expression) extends Aggregation

case class Calculation(expr: Expression) extends Aggregation

case class AggregationAdd(aggr1: Aggregation, aggr2: Aggregation) extends Aggregation

case class AggregationSub(aggr1: Aggregation, aggr2: Aggregation) extends Aggregation

case class AggregationMultiply(aggr1: Aggregation, aggr2: Aggregation) extends Aggregation

case class AggregationDivide(aggr1: Aggregation, aggr2: Aggregation) extends Aggregation

trait AggregationParsers extends ExpressionParsers with MyParsers{

  def aExpression: Parser[Aggregation] = aTerm ~ rep(("+" | "-") ~ aTerm) ^^ {
    case f ~ fs => fs.foldLeft(f) {
      case (acc, c) => AggregationAdd(acc, c._2)
      case (acc, c) => AggregationSub(acc, c._2)
    }
  }

  def aTerm: Parser[Aggregation] = aPrimary ~ rep(("*" | "/") ~ aPrimary) ^^ {
    case f ~ fs => fs.foldLeft(f) {
      case (acc, c) => AggregationMultiply(acc, c._2)
      case (acc, c) => AggregationDivide(acc, c._2)
    }
  }

  def aPrimary: Parser[Aggregation] = "(" ~ aExpression ~ ")" ^^ {
    case _ ~ aexpr ~ _ => aexpr
  } | aggregate

  def aggregate: Parser[Aggregation] = ("SUM" | "AVE" | "COUNT" | "MAX" | "MIN") ~ "(" ~ expression ~ ")" ^^ {
    case "SUM" ~ _ ~ expr ~ _=> SumAggregation(expr)
    case "AVE" ~ _ ~ expr ~ _=> AveAggregation(expr)
    case "COUNT" ~ _ ~ expr ~ _=> CountAggregation(expr)
    case "MAX" ~ _ ~ expr ~ _=> MaxAggregation(expr)
    case "MIN" ~ _ ~ expr ~ _=> MinAggregation(expr)
  } | expression ^^ {
    case expr => Calculation(expr)
  }
}
