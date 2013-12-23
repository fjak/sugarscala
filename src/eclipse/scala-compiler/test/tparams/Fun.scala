package tparams

trait Fun {
    def map[A, B](l: List[A])(f: A => B) = l.map(f)
}
