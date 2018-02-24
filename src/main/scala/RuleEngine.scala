package scala

import constant.Row
import parser._
import visitor.{Aggregator, Evaluator}

object RuleEngine extends App {

  val rows = Seq(
    Row(1, 2, 3, "tag1"),
    Row(4, 5, 6, "tag2"),
    Row(7, 8, 9, "tag3")
  )

  val rule1 = "0.8 * row.f1 + row.f2"
  val rule2 = "AVE(1.5 * row.f3)"
  val rule3 = s"$rule1 > $rule2 OR word: row.tag contains word: g1"

  class CondParser extends ConditionParsers
  val parser = new CondParser

  val exprResult = parser.parseAll(parser.expression, rule1)
  val aggrResult = parser.parseAll(parser.aExpression, rule2)
  val condResult = parser.parseAll(parser.conditions, rule3)

  println(exprResult)
  println(aggrResult)
  println(condResult)

  val evs = rows.map(new Evaluator(_))

  evs.foreach(ev => {
    if (exprResult.successful) {
      println(exprResult.get.accept(ev))
    }
    if (aggrResult.successful) {
      println(aggrResult.get.accept(new Aggregator(evs, ev)))
    }
    if (condResult.successful) {
      println(condResult.get.accept(new Aggregator(evs, ev)))
    }
  })
}
