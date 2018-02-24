package constant

case class Order(value: Object, arrangement: Option[Arrangement])

sealed trait Arrangement

object Arrangement {
  def fromValue(value: String) = value match {
    case "ASC" => ASC
    case "DESC" => DESC
  }
}

case object ASC extends Arrangement
case object DESC extends Arrangement

