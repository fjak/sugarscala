package scala.tools.nsc.sugar

case class Id(name: String) extends StrategoTerm
trait AnnotationSeq extends StrategoTerm
trait Modifier extends StrategoTerm
trait PackageDeclarationSemi extends StrategoTerm
trait PackageDeclaration extends StrategoTerm
case class CompilationUnit(
             pkgsDecls: List[PackageDeclarationSemi],
             stats: List[TopStatSemi]) extends StrategoTerm
case class TopStatSemi(stat: TopStat, terminator: String) extends StrategoTerm
trait TopStat extends StrategoTerm
case class TopTmplDef(
             annots: Option[AnnotationSeq],
             mods: List[Modifier],
             tmplDef: TmplDef) extends TopStat
trait TmplDef extends StrategoTerm
case class Object(objectDef: ObjectDef) extends TmplDef
case class ObjectDef(
             id: Id,
             classTemplateOpt: ClassTemplateOpt) extends StrategoTerm
trait ClassTemplateOpt extends StrategoTerm
case class EmptyClassTemplateOpt extends ClassTemplateOpt

object ParseTree {
  def apply(term: StrategoTerm) = compilationUnit(term)

  def ident(term: StrategoTerm) = term match {
    case Appl("Id", Str(id)) => Id(id)
    case _ => sys.error("Malformed Id: ${term}")
  }

  def option[T](term: StrategoTerm)(f: StrategoTerm => T) = term match {
    case Appl("None") => None
    case Appl("Some", term) => Some(f(term))
    case _ => sys.error(s"Malformed Option: ${term}")
  }

  def annotationSeq(term: StrategoTerm) = sys.error("Not implemented")

  def mod(t: StrategoTerm) = sys.error("Not implemented")

  def pkgDeclSemi(term: StrategoTerm) = sys.error("Not implemented")

  def pkgDecl(term: StrategoTerm) = sys.error("Not implemented")

  def compilationUnit(term: StrategoTerm): CompilationUnit = term match {
    case Appl("CompilationUnit", Lst(pkgDecls@_*), Lst(stats@_*)) =>
      compilationUnit(pkgDecls.toList, stats.toList)
    case _ => sys.error(s"Malformed CompilationUnit: ${term}")
  }

  def compilationUnit(
        pkgDeclSemis: List[StrategoTerm],
        topStatSemis: List[StrategoTerm]) =
    CompilationUnit(pkgDeclSemis map pkgDeclSemi, topStatSemis map topStatSemi)

  def topStatSemi(term: StrategoTerm) = term match {
    case Appl("TopStatSemi", t, Str(terminator)) =>
      TopStatSemi(topStat(t), terminator)
    case _ => sys.error(s"Malformed TopStatSemi: ${term}")
  }

  def topStat(term: StrategoTerm) = term match {
    case Appl("TopTmplDef", opt, Lst(mods@_*), tmpl) =>
      topTmplDef(opt, mods.toList, tmpl)
    case _ => sys.error(s"Malformed TopStat: ${term}")
  }

  def topTmplDef(
        opt: StrategoTerm,
        mods: List[StrategoTerm],
        tmpl: StrategoTerm) =
    TopTmplDef(
      option[AnnotationSeq](opt)(annotationSeq),
      mods map mod,
      tmplDef(tmpl))

  def tmplDef(term: StrategoTerm) = term match {
    case Appl("Object", t) => Object(objectDef(t))
    case _ => sys.error("Malformed TmplDef: ${term}")
  }

  def objectDef(term: StrategoTerm) = term match {
    case Appl("ObjectDef", id, t) =>
      ObjectDef(ident(id), classTemplateOpt(t))
  }

  def classTemplateOpt(term: StrategoTerm) = term match {
    case Appl("EmptyClassTemplateOpt") => EmptyClassTemplateOpt()
    case _ => sys.error("Malformed ClassTemplateOpt: ${term}")
  }
}
