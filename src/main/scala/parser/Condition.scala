package parser

trait ConditionVisitor {
  def visit(e: Condition): Boolean
}

trait Condition {
  def accept(visitor: ConditionVisitor): Boolean = {
    visitor.visit(this)
  }
}

case class OrCondition(cond1: Condition, cond2: Condition) extends Condition

case class AndCondition(cond1: Condition, cond2: Condition) extends Condition

case class NotCondition(cond: Condition) extends Condition

case class LTCondition(expr1: Aggregation, expr2: Aggregation) extends Condition

case class GTCondition(expr1: Aggregation, expr2: Aggregation) extends Condition

case class LECondition(expr1: Aggregation, expr2: Aggregation) extends Condition

case class GECondition(expr1: Aggregation, expr2: Aggregation) extends Condition

case class EQCondition(expr1: Aggregation, expr2: Aggregation) extends Condition

case class ContainsCondition(w1: Word, w2: Word) extends Condition

case class EqualsCondition(w1: Word, w2: Word) extends Condition


trait ConditionParsers extends AggregationParsers with WordParsers {

  def conditions: Parser[Condition] = or

  def or: Parser[Condition] = and | or ~ rep("OR" ~ or) ^^ {
    case a ~ as => as.foldLeft(a) {
      case (acc, at) => OrCondition(acc, at._2)
    }
  }

  def and: Parser[Condition] = cunary | and ~ rep("AND" ~ and) ^^ {
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

  def condition: Parser[Condition] = aExpression ~ ("<" | ">" | "<=" | ">=" | "==") ~ aExpression ^^ {
    case expr1 ~ "<" ~ expr2 => LTCondition(expr1, expr2)
    case expr1 ~ ">" ~ expr2 => GTCondition(expr1, expr2)
    case expr1 ~ "<=" ~ expr2 => LECondition(expr1, expr2)
    case expr1 ~ ">=" ~ expr2 => GECondition(expr1, expr2)
    case expr1 ~ "==" ~ expr2 => EQCondition(expr1, expr2)
  } | word ~ ("contains" | "equals") ~ word ^^ {
    case w1 ~ "contains" ~ w2 => ContainsCondition(w1, w2)
    case w1 ~ "equals" ~ w2 => EqualsCondition(w1, w2)
  }
}
