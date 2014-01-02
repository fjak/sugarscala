package closures

object NestedMaker {
    def mk(f: (Int, Int) => Int) = f
}

object Nested {
    val f: NestedMaker.type => ((Int, Int) => Int) = {_ mk {_ + _}}
}
