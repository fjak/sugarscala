package exprs

object MappingList {
  def main(args: Array[String]) {
    println(List(1, 2, 3) map ((x: Int) => x * 2))
  }
}
