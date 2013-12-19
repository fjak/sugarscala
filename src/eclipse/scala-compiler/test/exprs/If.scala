package exprs

object If {
  def main(args: Array[String]) {
    if (args(0).toBoolean) println("true")

    if (args(0).toBoolean) println("true")
    else println("false")
    println("Goodbye...")
  }
}
