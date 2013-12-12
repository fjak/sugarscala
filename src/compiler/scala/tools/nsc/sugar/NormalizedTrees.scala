package scala.tools.nsc.sugar

import org.kiama.rewriting.Rewriter._

trait NormValDef
case class NoValDef extends NormValDef

trait NormTemplateTrait extends StrategoTerm
case class EmptyTemplate extends NormTemplateTrait
case class NormTemplate(
  parents: List[StrategoTerm],
  valDef: NormValDef,
  body: StrategoTerm*) extends NormTemplateTrait

trait NormModifiers extends StrategoTerm
case class NoModifiers extends NormModifiers

trait NormPackage extends StrategoTerm
case class EmptyPackage(stats: StrategoTerm*) extends NormPackage

trait NormTypeTree extends StrategoTerm
case class NormUnit extends NormTypeTree
trait NormTypeDef extends StrategoTerm

case class NormDefDef(mods: NormModifiers, name: String, tparams: List[NormTypeDef],
             vparamss: List[List[NormValDef]], tpt: NormTypeTree, rhs: StrategoTerm) extends StrategoTerm

case class NormModuleDef(mods: NormModifiers, name: String, tpl: NormTemplateTrait) extends StrategoTerm

object NormalizedTree {
  def normMods(annots: Option[AnnotationSeq], mods: Modifier*) = {
    NoModifiers()
  }

  def normObjBody(cto: ClassTemplateOpt): NormTemplateTrait = cto match {
    case EmptyClassTemplateOpt() => EmptyTemplate()
    case TemplateBody(stats@_*) => NormTemplate(Nil, NoValDef(), stats:_*)
    case _ => sys.error(s"Could not normalize ${cto}")
  }

  def normProcedure(tplStat: TemplateStat) = tplStat match {
    case DefTemplateStat(
           annots,
           mods,
           FunDefDef(ProcDef(FunSig(Id(name), tpc, pc), block))) => {
      NormDefDef(
        normMods(annots, mods:_*),
        name,
        Nil,
        List(Nil),
        NormUnit(),
        block)
    }
  }

  val removeSemi = rule {
    case TopStatSemi(t, _) => t
    case TemplateStatSemi(t, _) => t
    case BlockStatSemi(t, _) => t
  }

  val topStatToModuleDef = rule {
    case TopTmplDef(annots, mods, Object(ObjectDef(name, body))) =>
      NormModuleDef(normMods(annots, mods:_*), name.name, normObjBody(body))
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
