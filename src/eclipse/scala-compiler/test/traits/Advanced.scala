package traits

trait Foo {
  def f(i: Int): Int
}

trait Bar {
  def g(s: String): String
}

trait Baz

trait FooBar extends Foo with Bar with Baz

class FooBarImpl {
  def f(i: Int): Int = i
  def g(s: String): String = s
}
