package scala.tools.nsc.sugar

import org.kiama.rewriting.Rewriter._

object NormalizedTree {
  val removeSemi = rule {
    case TopStatSemi(t, _) => t
  }

  def apply(tree: ParseTree) = rewrite(alltd(removeSemi))(tree)
}
