package types

import scala.collection.mutable.HashMap

class TypeApplication[A, B] {
  def empty: HashMap[A, B] = HashMap.empty[A, B]
}
