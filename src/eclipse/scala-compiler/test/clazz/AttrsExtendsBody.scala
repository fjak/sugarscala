package clazz

class Foo(i: Int)

class Bar(i: Int, j: Int) extends Foo(i)

class Baz(i: Int, j: Int, s: String) extends Bar(i, j) {
  override def toString = s
}
