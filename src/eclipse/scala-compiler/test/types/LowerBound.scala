package types

trait LowerBoundFooable {
  def foo
}

trait LowerBound[A >: LowerBoundFooable]
