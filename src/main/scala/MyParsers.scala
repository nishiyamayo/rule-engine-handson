package parser

import scala.util.parsing.combinator.JavaTokenParsers

trait MyParsers extends JavaTokenParsers {
  def reWord: Parser[String] = "[a-zA-Z][a-zA-Z0-9_]*".r
  def reVariable: Parser[~[~[String, String], String]] = "row"~"."~reWord
}
