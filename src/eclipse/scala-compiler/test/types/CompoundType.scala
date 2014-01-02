package types

trait CompoundTypeFoo

trait CompoundType[A] {
  def f(a: A with CompoundTypeFoo)
}
