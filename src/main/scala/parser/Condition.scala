package parser

trait ConditionVisitor {
  def visit(c: Condition): Boolean
}

trait Condition {
  def accept(visitor: ConditionVisitor): Boolean = {
    visitor.visit(this)
  }
}

case class OrCondition(cond1: Condition, cond2: Condition) extends Condition

case class AndCondition(cond1: Condition, cond2: Condition) extends Condition

case class NotCondition(cond: Condition) extends Condition

case class LTCondition(aggr1: Aggregation, aggr2: Aggregation) extends Condition

case class GTCondition(aggr1: Aggregation, aggr2: Aggregation) extends Condition

case class LECondition(aggr1: Aggregation, aggr2: Aggregation) extends Condition

case class GECondition(aggr1: Aggregation, aggr2: Aggregation) extends Condition

case class EQCondition(aggr1: Aggregation, aggr2: Aggregation) extends Condition

case class ContainsCondition(w1: Word, w2: Word) extends Condition

case class EqualsCondition(w1: Word, w2: Word) extends Condition


trait ConditionParsers extends AggregationParsers with WordParsers {

  def conditions: Parser[Condition] = or

  def or: Parser[Condition] = and ~ rep("OR" ~ and) ^^ {
    case a ~ as => as.foldLeft(a) {
      case (acc, at) => OrCondition(acc, at._2)
    }
  }

  def and: Parser[Condition] = cunary ~ rep("AND" ~ cunary) ^^ {
    case a ~ as => as.foldLeft(a) {
      case (acc, at) => AndCondition(acc, at._2)
    }
  }

  def cunary: Parser[Condition] = "NOT" ~ cprimary ^^ {
    case _ ~ cp => NotCondition(cp)
  } | cprimary

  def cprimary: Parser[Condition] = "(" ~ conditions ~ ")" ^^ {
    case _ ~ cs ~ _ => cs
  } | condition

  def condition: Parser[Condition] = aExpression ~ ("<=" | ">=" | "<" | ">" | "==") ~ aExpression ^^ {
    case aggr1 ~ "<=" ~ aggr2 => LECondition(aggr1, aggr2)
    case aggr1 ~ ">=" ~ aggr2 => GECondition(aggr1, aggr2)
    case aggr1 ~ "<" ~ aggr2 => LTCondition(aggr1, aggr2)
    case aggr1 ~ ">" ~ aggr2 => GTCondition(aggr1, aggr2)
    case aggr1 ~ "==" ~ aggr2 => EQCondition(aggr1, aggr2)
  } | word ~ ("contains" | "equals") ~ word ^^ {
    case w1 ~ "contains" ~ w2 => ContainsCondition(w1, w2)
    case w1 ~ "equals" ~ w2 => EqualsCondition(w1, w2)
  }
}
