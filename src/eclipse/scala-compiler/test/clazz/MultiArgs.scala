package clazz

class MultiArgsFoo(i: Int, j: Int)(s: String)(b: Boolean)

class MultiArgs(i: Int, j: Int)(s: String)(b: Boolean) extends MultiArgsFoo(i, j)(s)(b)
