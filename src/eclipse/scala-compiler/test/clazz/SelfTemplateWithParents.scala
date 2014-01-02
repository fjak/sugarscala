package clazz

trait SelfTemplateWithParentsFoo

trait SelfTemplateWithParents extends SelfTemplateWithParentsFoo { self =>
  def f = self.toString
}
