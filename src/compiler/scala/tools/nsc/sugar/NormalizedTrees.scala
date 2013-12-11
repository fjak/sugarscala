package scala.tools.nsc.sugar

import org.kiama.rewriting.Rewriter._

trait Template extends StrategoTerm
case class EmptyTemplate extends Template

trait Modifiers extends StrategoTerm
case class NoModifiers extends Modifiers

trait Package extends StrategoTerm
case class EmptyPackage(stats: List[StrategoTerm]) extends Package

case class
ModuleDef(mods: Modifiers, name: String, tpl: Template) extends StrategoTerm

object NormalizedTree {
  def normMods(annots: Option[AnnotationSeq], mods: Seq[Modifier]) = {
    NoModifiers()
  }

  def normObjBody(cto: ClassTemplateOpt) = cto match {
    case EmptyClassTemplateOpt() => EmptyTemplate()
    case _ => sys.error(s"Could not")
  }

  val removeSemi = rule {
    case TopStatSemi(t, _) => t
  }

  val topStatToModuleDef = rule {
    case TopTmplDef(annots, mods, Object(ObjectDef(name, body))) =>
      ModuleDef(normMods(annots, mods), name.name, normObjBody(body))
  }

  val normCompilationUnit = rule {
    case CompilationUnit(Nil, stats) => EmptyPackage(stats)
  }

  def apply(tree: StrategoTerm) = {
    val ph1 = rewrite(alltd(removeSemi))(tree)
    val ph2 = rewrite(alltd(topStatToModuleDef))(ph1)
    rewrite(normCompilationUnit)(ph2)
  }
}
