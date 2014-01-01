package clazz

abstract class NewAnon {
  val foo: Int
}

object NewAnon extends App {
  new NewAnon { val foo = 42 }
}
