package exprs

import scala.collection.mutable.Map

object OpAssign {
  val m = Map[Int, String]()
  m ++= Map(1 -> "foo", 2 -> "bar", 3 -> "baz") filter (_._1 != 2)
}
