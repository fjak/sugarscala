package exprs

import scala.collection.mutable.Map

object AccessAssignment {
  val m = Map[String, Int]()
  m("foo") = 42
}
