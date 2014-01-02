package exprs

object DesigAssignFoo {
  var foo: Int = -1
}

object DesigAssign {
  val f = DesigAssignFoo
  f.foo = 42
}
