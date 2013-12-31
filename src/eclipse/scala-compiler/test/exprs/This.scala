package exprs

class This {
  val foo = "42"
  println(This.this.foo)
}
