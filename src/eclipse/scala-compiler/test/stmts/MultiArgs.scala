package clazz

class MultiArgs {
  def f(i: Int, j: Int)(s: String, s2: String)(b: Boolean, b1: Boolean) = i
  f(23, 42)("Hello", "World")(true, false)
}
