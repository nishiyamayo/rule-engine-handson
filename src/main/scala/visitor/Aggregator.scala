package visitor

import parser._

class Aggregator(evs: Seq[Evaluator], ev: Evaluator) extends AggregationVisitor with ConditionVisitor with LimitVisitor {

  override def visit(a: Aggregation): Double = a match {
    case SumAggregation(expression: Expression) => evs.map(expression.accept(_)).sum

    case AveAggregation(expression: Expression) => evs.map(expression.accept(_)).sum / evs.length

    case CountAggregation(expression: Expression) => evs.length

    case MaxAggregation(expression: Expression) => evs.map(expression.accept(_)).max

    case MinAggregation(expression: Expression) => evs.map(expression.accept(_)).min

    case Calculation(expr: Expression) => expr.accept(ev)

    case AggregationAdd(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) + aggr2.accept(this)

    case AggregationSub(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) - aggr2.accept(this)

    case AggregationMultiply(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) * aggr2.accept(this)

    case AggregationDivide(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) / aggr2.accept(this)
  }

  override def visit(c: Condition): Boolean = c match {
      
    case OrCondition(cond1: Condition, cond2: Condition) => cond1.accept(this) || cond2.accept(this)

    case AndCondition(cond1: Condition, cond2: Condition) => cond1.accept(this) || cond2.accept(this)

    case NotCondition(cond: Condition) => !cond.accept(this)

    case LTCondition(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) < aggr2.accept(this)

    case GTCondition(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) > aggr2.accept(this)

    case LECondition(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) <= aggr2.accept(this)

    case GECondition(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) >= aggr2.accept(this)

    case EQCondition(aggr1: Aggregation, aggr2: Aggregation) => aggr1.accept(this) == aggr2.accept(this)

    case ContainsCondition(w1: Word, w2: Word) => w1.accept(ev).contains(w2.accept(ev))

    case EqualsCondition(w1: Word, w2: Word) => w1.accept(ev).equals(w2.accept(ev))

  }

  override def visit(l: Limit): Int = l match {
    case AggregationLimit(aggr: Aggregation) => aggr.accept(this).toInt
  }
}
