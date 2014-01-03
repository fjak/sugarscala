package closures

object InTuple {
  def f(s: String, l: List[String]) =
    l filter (s != _)
}
