package scala.tools.nsc.sugar

import org.kiama.rewriting.Rewriter._

trait Template extends StrategoTerm
case class EmptyTemplate extends Template

trait Modifiers extends StrategoTerm
case class NoModifiers extends Modifiers

trait Package extends StrategoTerm
case class EmptyPackage(stats: StrategoTerm*) extends Package

case class
ModuleDef(mods: Modifiers, name: String, tpl: Template) extends StrategoTerm

object NormalizedTree {
  def normMods(annots: Option[AnnotationSeq], mods: Modifier*) = {
    NoModifiers()
  }

  def normObjBody(cto: ClassTemplateOpt) = cto match {
    case EmptyClassTemplateOpt() => EmptyTemplate()
    case _ => sys.error(s"Could not normalize ${cto}")
  }

  val removeSemi = rule {
    case TopStatSemi(t, _) => t
    case TemplateStatSemi(t, _) => t
    case BlockStatSemi(t, _) => t
  }

  val topStatToModuleDef = rule {
    case TopTmplDef(annots, mods, Object(ObjectDef(name, body))) =>
      ModuleDef(normMods(annots, mods:_*), name.name, normObjBody(body))
  }

  val normCompilationUnit = rule {
    case CompilationUnit(Nil, stats@_*) => EmptyPackage(stats:_*)
  }

  def apply(tree: StrategoTerm) = {
    val ph1 = rewrite(innermost(removeSemi))(tree)
    val ph2 = rewrite(alltd(topStatToModuleDef))(ph1)
    rewrite(normCompilationUnit)(ph2)
  }
}
