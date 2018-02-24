package parser

import constant.{Arrangement, Order}



trait OrderParsers extends ExpressionParsers with WordParsers{
  def orderBy: Parser[Seq[Order]] = "ORDER BY" ~ order ~ rep("," ~ order) ^^ {
    case _ ~ or ~ ors =>
      Seq.concat(Seq(or), ors.seq.map(_._2))
  }

  def order: Parser[Order] = (expression | word) ~ opt("ASC" | "DESC") ^^ {
    case expr ~ arOpt => Order(expr, arOpt.map(ar => Arrangement.fromValue(ar)))
    case word ~ arOpt => Order(word, arOpt.map(ar => Arrangement.fromValue(ar)))
  }
}
