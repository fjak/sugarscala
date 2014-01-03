package types

trait ByNameParam {
  def foo(f: => String)
}
