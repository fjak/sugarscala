package tparams

trait NoArgFun {
  def foo[A](f: () => A) = f()
}
