package exprs.patterns

class Typed(o: Any) {
  o match {
    case some: String => println(some)
  }
}
