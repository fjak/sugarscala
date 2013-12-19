package exprs.patterns

object Const {
  def matchStr(s: String) = s match {
    case "foo" => "You said foo"
    case "bar" => "You said bar"
    case _ => "What?"
  }

  def main(args: Array[String]) {
    println(matchStr(args(0)))
  }
}
