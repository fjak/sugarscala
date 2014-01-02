package clazz

trait SelfTemplate { self =>
  def f = self.toString
}
