package scala.tools.nsc.sugar

trait AnnotationSeq extends StrategoTerm
trait Modifier extends StrategoTerm
trait PackageDeclarationSemi extends StrategoTerm
trait PackageDeclaration extends StrategoTerm

trait StableId extends StrategoTerm
case class Id(name: String) extends StableId

case class CompilationUnit(
             pkgsDecls: List[PackageDeclarationSemi],
             stats: TopStatSemi*) extends StrategoTerm

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
case class TemplateBody(stats: TemplateStatSemi*) extends ClassTemplateOpt
case class TemplateStatSemi(stat: TemplateStat, terminator: String) extends StrategoTerm
trait TemplateStat extends StrategoTerm
case class DefTemplateStat(
             annots: Option[AnnotationSeq],
             mods: List[Modifier],
             `def`: Def) extends TemplateStat

trait Def extends StrategoTerm
case class FunDefDef(funDef: FunDef) extends Def
trait FunDef extends StrategoTerm
case class ProcDef(sig: FunSig, block: Block) extends FunDef
case class FunSig(id: Id, tpc: Option[TypeParamClause], pc: Option[ParamClausesTrait]) extends StrategoTerm
trait VariantTypeParam extends StrategoTerm

case class Annotation(`type`: SimpleType, args: Option[ArgumentExprsSeq])

case class TypeParamClause(vtps: VariantTypeParam*) extends StrategoTerm
trait ParamClausesTrait
case class ParamClause(params: Param*) extends ParamClausesTrait
case class Param(
             annots: List[Annotation],
             id: Id,
             paramTyped: Option[ParamTyped],
             assignment: Option[Assignment]) extends StrategoTerm
case class ParamTyped(paramType: ParamType) extends StrategoTerm
trait ParamType extends StrategoTerm
case class ParameterizedType(`type`: SimpleType, typeArgs: TypeArgs) extends ParamType

case class BlockStatSemi(blockStat: BlockStat, terminator: String) extends StrategoTerm
case class Block(blockStatSemis: BlockStatSemi*) extends StrategoTerm

case class TypeArgs(types: Type*) extends StrategoTerm
trait SimpleType extends StrategoTerm
case class Type(stableId: StableId) extends SimpleType

trait ArgumentExprsTrait extends StrategoTerm
case class ArgumentExprs(exprs: Expr*) extends ArgumentExprsTrait
case class ArgumentExprsSeq(argsExprs: ArgumentExprs*) extends StrategoTerm

trait BlockStat extends StrategoTerm
trait Expr extends BlockStat
object Expr {
  def unapply(t: StrategoTerm) = t match {
    case e@Appl("AppExpr", _@_*) => Some(e)
    case _ => None
  }
}
case class Assignment(expr: Expr) extends Expr
case class AppExpr(id: Id, argExprs: ArgumentExprs) extends Expr
case class StringLiteralExpr(s: String) extends Expr

object ParseTree {
  def apply(term: StrategoTerm) = compilationUnit(term)

  def compilationUnit(term: StrategoTerm): CompilationUnit = term match {
    case Appl("CompilationUnit", Lst(pkgDecls@_*), Lst(stats@_*)) =>
      compilationUnit(pkgDecls.toList, stats.toList)
    case _ => sys.error(s"Malformed CompilationUnit: ${term}")
  }

  def compilationUnit(
        pkgDeclSemis: List[StrategoTerm],
        topStatSemis: List[StrategoTerm]) =
    CompilationUnit(pkgDeclSemis map pkgDeclSemi, topStatSemis map topStatSemi:_*)

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
    case _ => sys.error(s"Malformed TmplDef: ${term}")
  }

  def objectDef(term: StrategoTerm) = term match {
    case Appl("ObjectDef", id, t) =>
      ObjectDef(ident(id), classTemplateOpt(t))
  }

  def classTemplateOpt(term: StrategoTerm) = term match {
    case Appl("EmptyClassTemplateOpt") => EmptyClassTemplateOpt()
    case Appl("TemplateBody", Lst(stats@_*)) => TemplateBody(stats map templateStatSemi:_*)
    case _ => sys.error(s"Malformed ClassTemplateOpt: ${term}")
  }

  def templateStatSemi(term: StrategoTerm) = term match {
    case Appl("TemplateStatSemi", tmplStat, Str(terminator)) =>
      TemplateStatSemi(templateStat(tmplStat), terminator)
    case _ => sys.error(s"Malformed TemplateStatSemi: ${term}")
  }

  def templateStat(term: StrategoTerm) = term match {
    case Appl("DefTemplateStat", annots, Lst(mods@_*), def_) =>
      DefTemplateStat(
        option[AnnotationSeq](annots)(annotationSeq),
        mods.toList map mod,
        `def`(def_))
    case _ => sys.error(s"Malformed TemplateStat: ${term}")
  }

  def `def`(term: StrategoTerm) = term match {
    case Appl("FunDefDef", funDef_) => FunDefDef(funDef(funDef_))
    case _ => sys.error(s"Malformed Def: ${term}")
  }

  def funDef(term: StrategoTerm) = term match {
    case Appl("ProcDef", funSig_, block_) =>
      ProcDef(funSig(funSig_), block(block_))
    case _ => sys.error(s"Malformed FunDef: ${term}")
  }

  def funSig(term: StrategoTerm) = term match {
    case Appl("FunSig", id, tpc_, pc_) =>
      FunSig(
        ident(id),
        option[TypeParamClause](tpc_)(tpc),
        option[ParamClausesTrait](pc_)(pc))
    case _ => sys.error(s"Malformed FunSig: ${term}")
  }

  def block(term: StrategoTerm) = term match {
    case Appl("Block", Lst(blockStatSemis@_*)) =>
      Block(blockStatSemis map blockStatSemi:_*)
    case _ => sys.error(s"Malformed Block: ${term}")
  }

  def blockStatSemi(term: StrategoTerm) = term match {
    case Appl("BlockStatSemi", blockStat_, Str(terminator)) =>
      BlockStatSemi(blockStat(blockStat_), terminator)
    case _ => sys.error(s"Malformed BlockStatSemi: ${term}")
  }


  def expr(term: StrategoTerm): Expr = term match {
    case Appl("AppExpr", id, argExprs) =>
      AppExpr(ident(id), argumentExprs(argExprs))
    case Appl("String", Str(s)) => StringLiteralExpr(s)
    case _ => sys.error(s"Malformed Expr: ${term}")
  }

  def blockStat(term: StrategoTerm) = term match {
    case Expr(term) => expr(term)
    case _ => sys.error(s"Malformed BlockStat: ${term}")
  }

  def assignment(term: StrategoTerm) = term match {
    case _ => sys.error(s"Malformed Assignment: ${term}")
  }

  def argumentExprs(term: StrategoTerm) = term match {
    case Appl("ArgumentExprs", exprsOpt) => exprsOpt match {
      case Appl("Some", Appl("Exprs", Lst(exprs@_*))) =>
        ArgumentExprs(exprs map expr:_*)
      case Appl("None") => ArgumentExprs()
    }
    case _ => sys.error(s"Malformed ArgumentExprs: ${term}")
  }

  def pc(term: StrategoTerm) = term match {
    case Appl("ParamClause", Lst(params@_*)) =>
      ParamClause(params.toList map param:_*)
    case _ => sys.error(s"Malformed ParamClauses: ${term}")
  }

  def param(term: StrategoTerm) = term match {
    case Appl("Param", Lst(annots@_*), id, paramTyped_, assignment_) =>
      Param(
        annots.toList map annot,
        ident(id),
        option[ParamTyped](paramTyped_)(paramTyped),
        option[Assignment](assignment_)(assignment))
    case _ => sys.error(s"Malformed Param: ${term}")
  }

  def paramTyped(term: StrategoTerm) = term match {
    case Appl("ParamTyped", paramType_) => ParamTyped(paramType(paramType_))
    case _ => sys.error(s"Malformed ParamTyped: ${term}")
  }


  def paramType(term: StrategoTerm) = term match {
    case Appl("ParameterizedType", typ_, typeArgs_) =>
      ParameterizedType(simpleType(typ_), typeArgs(typeArgs_))
    case _ => sys.error(s"Malformed ParamType: ${term}")
  }

  def simpleType(term: StrategoTerm) = term match {
    case Appl("Type", stableId_) => Type(stableId(stableId_))
    case _ => sys.error(s"Malformed SimpleType: ${term}")
  }

  def typeArgs(term: StrategoTerm) = term match {
    case Appl("TypeArgs", Lst(types@_*)) =>
      TypeArgs(types map `type`:_*)
    case _ => sys.error(s"Malformed TypeArgs: ${term}")
  }

  def `type`(term: StrategoTerm) = term match {
    case Appl("Type", id) => Type(ident(id))
    case _ => sys.error(s"Malformed Type: ${term}")
  }

  def annot(term: StrategoTerm) = term match {
    case _ => sys.error(s"Malformed Annotation: ${term}")
  }
  def annotationSeq(term: StrategoTerm) = sys.error("Not implemented")
  def mod(t: StrategoTerm): Modifier = sys.error("Not implemented")
  def pkgDeclSemi(term: StrategoTerm) = sys.error("Not implemented")
  def pkgDecl(term: StrategoTerm) = sys.error("Not implemented")
  def tpc(term: StrategoTerm) = term match {
    case _ => sys.error(s"Malformed TypeParamClause: ${term}")
  }

  def option[T](term: StrategoTerm)(f: StrategoTerm => T) = term match {
    case Appl("None") => None
    case Appl("Some", term) => Some(f(term))
    case _ => sys.error(s"Malformed Option: ${term}")
  }

  def ident(term: StrategoTerm) = term match {
    case Appl("Id", Str(id)) => Id(id)
    case _ => sys.error("Malformed Id: ${term}")
  }

  def stableId(term: StrategoTerm) = term match {
    case Appl("Id", Str(id)) => Id(id)
    case _ => sys.error(s"Malformed StableId: ${term}")
  }
}
