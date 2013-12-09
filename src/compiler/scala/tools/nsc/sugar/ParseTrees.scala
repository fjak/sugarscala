package scala.tools.nsc.sugar

trait ParseTrees extends StrategoTerms {

  trait ParseTree
  object ParseTree {
    def apply(term: StrategoTerm) = compilationUnit(term)
  }

  case class Id(name: String) extends ParseTree
  def ident(term: StrategoTerm) = term match {
    case Appl("Id", Str(id)) => Id(id)
    case _ => sys.error("Malformed Id: ${term}")
  }

  def option[T](term: StrategoTerm)(f: StrategoTerm => T) = term match {
    case Appl("None") => None
    case Appl("Some", term) => Some(f(term))
    case _ => sys.error(s"Malformed Option: ${term}")
  }

  trait AnnotationSeq extends ParseTree
  def annotationSeq(term: StrategoTerm) = sys.error("Not implemented")

  trait Modifier extends ParseTree
  def mod(t: StrategoTerm) = sys.error("Not implemented")

  trait PackageDeclarationSemi extends ParseTree
  def pkgDeclSemi(term: StrategoTerm) = sys.error("Not implemented")

  trait PackageDeclaration extends ParseTree
  def pkgDecl(term: StrategoTerm) = sys.error("Not implemented")

  case class CompilationUnit(
               pkgsDecls: Seq[PackageDeclarationSemi],
               stats: Seq[TopStatSemi]) extends ParseTree
  def compilationUnit(term: StrategoTerm): CompilationUnit = term match {
    case Appl("CompilationUnit", Lst(pkgDecls@_*), Lst(stats@_*)) =>
      compilationUnit(pkgDecls, stats)
    case _ => sys.error(s"Malformed CompilationUnit: ${term}")
  }
  def compilationUnit(
        pkgDeclSemis: Seq[StrategoTerm],
        topStatSemis: Seq[StrategoTerm]) =
    CompilationUnit(pkgDeclSemis map pkgDeclSemi, topStatSemis map topStatSemi)

  case class TopStatSemi(stat: TopStat, terminator: String) extends ParseTree
  def topStatSemi(term: StrategoTerm) = term match {
    case Appl("TopStatSemi", t, Str(terminator)) =>
      TopStatSemi(topStat(t), terminator)
    case _ => sys.error(s"Malformed TopStatSemi: ${term}")
  }

  trait TopStat extends ParseTree
  def topStat(term: StrategoTerm) = term match {
    case Appl("TopTmplDef", opt, Lst(mods@_*), tmpl) =>
      topTmplDef(opt, mods, tmpl)
    case _ => sys.error(s"Malformed TopStat: ${term}")
  }

  case class TopTmplDef(
               annots: Option[AnnotationSeq],
               mods: Seq[Modifier],
               tmplDef: TmplDef) extends TopStat
  def topTmplDef(
        opt: StrategoTerm,
        mods: Seq[StrategoTerm],
        tmpl: StrategoTerm) =
    TopTmplDef(
      option[AnnotationSeq](opt)(annotationSeq),
      mods map mod,
      tmplDef(tmpl))

  trait TmplDef extends ParseTree
  case class Object(objectDef: ObjectDef) extends TmplDef
  def tmplDef(term: StrategoTerm) = term match {
    case Appl("Object", t) => Object(objectDef(t))
    case _ => sys.error("Malformed TmplDef: ${term}")
  }

  case class ObjectDef(
               id: Id,
               classTemplateOpt: ClassTemplateOpt) extends ParseTree
  def objectDef(term: StrategoTerm) = term match {
    case Appl("ObjectDef", id, t) =>
      ObjectDef(ident(id), classTemplateOpt(t))
  }

  trait ClassTemplateOpt extends ParseTree
  case class EmptyClassTemplateOpt extends ClassTemplateOpt
  def classTemplateOpt(term: StrategoTerm) = term match {
    case Appl("EmptyClassTemplateOpt") => EmptyClassTemplateOpt()
    case _ => sys.error("Malformed ClassTemplateOpt: ${term}")
  }
}
