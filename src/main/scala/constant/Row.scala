package constant

case class Row(feature1: Long, feature2: Long, feature3: Long, tag: String) {

  def getDouble(field: String) = field match {
    case "f1" => feature1
    case "f2" => feature2
    case "f3" => feature3
  }

  def getString(field: String) = field match {
    case "tag" => tag
  }
}
