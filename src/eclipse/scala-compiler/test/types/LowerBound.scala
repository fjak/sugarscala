package types

trait UpperBoundFooable {
  def foo
}

trait UpperBound[A >: UpperBoundFooable]
