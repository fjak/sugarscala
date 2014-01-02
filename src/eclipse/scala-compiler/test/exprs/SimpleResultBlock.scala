package exprs

object SimpleResultBlock {
    List(1, 2, 3) map {
      println("Bananas")
      e => (e, e.toString)
    }
}
