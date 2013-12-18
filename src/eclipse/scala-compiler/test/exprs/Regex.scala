package exprs

object Regex extends App {
  println("Matching \"foobarbaz\" against /bar/")
  println("bar".r.findFirstIn("foobarbaz"))
}
