package exprs

class NamedCall {
  def f(i: Int, s: String = "Hello", s2: String = "World") = 42
  f(23, s2 = "bar")
}
