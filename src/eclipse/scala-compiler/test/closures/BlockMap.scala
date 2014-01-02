package closures

object BlockMap {
  println(List(1, 2, 3) map {_ * 2})
  println(List(1, 2, 3) map {2 * _})
}
