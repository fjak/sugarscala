package scala.tools.nsc
package sugar

import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.terms._
import org.spoofax.jsglr.client.imploder.TreeBuilder
import org.spoofax.jsglr.io.SGLR
import java.io.FileInputStream
import java.io.File
import scala.reflect.internal.util.SourceFile
import java.io.FileReader
import java.io.InputStream
import scala.reflect.internal.ModifierFlags

trait SGLRParsers {
  val global : Global
  import global._
  import treeBuilder.{global => _, _}

  def unescape(s: String): String = {
    s.drop(1).dropRight(1).replaceAllLiterally("""\"""", "\"")
  }

  val scala_tbl_stream =
    getClass.getResourceAsStream("/scala/tools/nsc/sugar/Scala.tbl")
  val scala_tbl = ParseTableManager.loadFromStream(scala_tbl_stream)
  val parser = SGLRParser

  case class IUnfinishedTemplate(parents: List[Tree], self: Option[ValDef], body: Tree*) extends Tree
  case class IObjectDef(mods: Modifiers, name: TermName, tpl: Option[Tree]) extends Tree
  case class IClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Option[Tree]) extends Tree

  def toTree(term: Term): Tree = term match {
    case "CompilationUnit" @@ (Lst(pkgs@_*), topStats) => toPackageDef(pkgs.toList, topStats)

    case "TopStatSemi" @@ (topStat, _) => toTree(topStat)

    case "TemplateStatSemi" @@ (stat, _) => toTree(stat)

    case "TopTmplDef" @@ (mods, annots, "Object" @@ ("ObjectDef" @@ (name, body))) =>
      IObjectDef(toModifiers(mods, annots), toTermName(name), toTemplate(body))

    case "TopTmplDef" @@
           (mods, annots, "Class" @@
              ("ClassDef" @@ (morphism, constrAnnots, accessMods, classParamClauses, tplOpt))) =>
      IClassDef(toModifiers(mods, annots, accessMods), toTypeName(morphism), toTypeDefs(morphism), toTemplate(tplOpt))

    case "DefTemplateStat" @@ (mods, annots, "FunDefDef" @@ funDef) =>
      toDefDef(toModifiers(mods, annots), funDef)

    case "Some" @@ (t) => toTree(t)

    case @@("None") => EmptyTree

    case "BlockExpr" @@ t => toTree(t)

    case "Block" @@ (t) => Block(toTrees(t):_*)

    case "BlockStatSemi" @@ (t, _) => toTree(t)

    case "AppExpr" @@ (fun, args) => Apply(toTree(fun), toTrees(args))

    case "Id" @@ Str(name) => Ident(name)

    case "String" @@ Str(s) => Literal(Constant(unescape(s)))

    case "Int" @@ Str(s) => Literal(Constant(s.toInt))

    case @@("False") => Literal(Constant(false))

    case t @ @@("ImportExpr", _*) => toImport(t)

    case t @ @@("WildcardImportExpr", _*) => toImport(t)

    case t @ @@("SelectorsImportExpr", _*) => toImport(t)

    case "StableId" @@ (qid, select) => Select(toTree(qid), toTermName(select))

    case Lst(t, ts@_*) => ts.foldLeft(toTree(t)) {(b,a) => Select(b, toTermName(a))}

    case Str(name) => Ident(name)

    case "Constr" @@ (typ, args) => toTypeTree(typ)

    case "ExprTemplateStat" @@ t => toTree(t)

    case "PrefixExpr" @@ (op, arg) =>
      Select(toTree(arg), toTermName(op).encode.prepend("unary_"))

    case "InfixExpr" @@ (lhs, op, rhs) =>
      Apply(Select(toTree(lhs), toTermName(op).encode), List(toTree(rhs)))

    case "PostfixExpr" @@ (arg, op) => Select(toTree(arg), toTermName(op).encode)

    case "DesignatorExpr" @@ (t, sel) => Select(toTree(t), toTermName(sel))

    case "TupleExpr" @@ (Lst(t)) => toTree(t)

    case "FunExpr" @@ (bindings, body) => Function(toValDefs(bindings), toTree(body))

    case "DefTemplateStat" @@ (mods, annots, "ValPatDef" @@ ("PatDef" @@ (Lst(name), typ, expr))) =>
      ValDef(toModifiers(mods, annots), toTermName(name), toTypeTree(typ), toTree(expr))

    case "DefTemplateStat" @@ (mods, annots, "VarPatDef" @@ ("PatDef" @@ (Lst(name), typ, expr))) =>
      ValDef(toModifiers(mods, annots) | ModifierFlags.MUTABLE, toTermName(name), toTypeTree(typ), toTree(expr))

    case "AssignmentExpr" @@ (lhs, rhs) => Assign(toTree(lhs), toTree(rhs))

    case "Path" @@ l => toTree(l)

    case "IfExpr" @@ (cond, then) => If(toTree(cond), toTree(then), EmptyTree)

    case "IfElseExpr" @@ (cond, then, els) => If(toTree(cond), toTree(then), toTree(els))

    case _ => sys.error(s"Can not translate term ${term} to Tree")
  }

  def toTrees(term: Term): List[Tree] = term match {
    case Lst() => Nil
    case Lst(t, ts@_*) => t match {
      case "TopStatSemi" @@ (imprt @ @@("Import", _*), _) => toTrees(Lst((imprt +: ts):_*))
      case "Import" @@ (imports) => toTrees(imports) ::: toTrees(Lst(ts:_*))
      case _ => toTree(t) :: toTrees(Lst(ts:_*))
    }
    case "ArgumentExprs" @@ t => toTrees(t)
    case "Some" @@ t => toTrees(t)
    case @@("None") => Nil
    case "Exprs" @@ t => toTrees(t)
    case "ClassParents" @@ (constr, annots) => toTree(constr) :: toTrees(annots)
    case "TemplateBody" @@ tplStatSemis => toTrees(tplStatSemis)
    case _ => sys.error(s"Can not transform ${term} to List[Tree]")
  }

  def toImport(term: Term): Import = term match {
    case "WildcardImportExpr" @@ sid =>
      Import(toTree(sid), ImportSelector.wildList)
    case "ImportExpr" @@ t => toImport(t)
    case "StableId" @@ (t, s) => Import(toTree(t), toImportSelectors(s))
    case "SelectorsImportExpr" @@ (sid, selects) =>
      Import(toTree(sid), toImportSelectors(selects))
    case _ => sys.error(s"Can not transform ${term} to Import")
  }

  def toImportSelectors(term: Term): List[ImportSelector] = term match {
    case Str(name) => {
      val typName = newTypeName(name)
      List(ImportSelector(typName, -1, typName, -1))
    }
    case "ImportSelectors" @@ Lst(t@_*) => (t map toImportSelector).toList
    case _ => sys.error(s"Can not transform ${term} to ImportSelectors")
  }

  def toImportSelector(term: Term): ImportSelector = term match {
    case "ImportSelector" @@ t => {
      val name = toTermName(t)
      ImportSelector(name, -1, name, -1)
    }
    case _ => sys.error(s"Can not transform ${term} to ImportSelector")
  }

  def toDefDef(mods: Modifiers, funDef: Term): DefDef = funDef match {
    case "ProcDef" @@ ("FunSig" @@ (id, tParams, vParams), body) =>
      DefDef(mods, toTermName(id), toTypeDefs(tParams), toValDefss(vParams),
             TypeTree(), toTree(body))
    case "FunDef" @@ ("FunSig" @@ (id, tParams, vParams), retType, body) =>
      DefDef(mods, toTermName(id), toTypeDefs(tParams), toValDefss(vParams),
             toTypeTree(retType), toTree(body))
    case _ => sys.error(s"Can not translate ${funDef} to DefDef")
  }

  def toTypeTree(term: Term): Tree = term match {
    case @@("None") => TypeTree()
    case "Some" @@ (t) => toTypeTree(t)
    case "Typed" @@ t => toTypeTree(t)
    case "Type" @@ t => toTypeTree(t)
    case "Id" @@ Str(name) => Ident(newTypeName(name))
    case "ParamTyped" @@ (t) => toTypeTree(t)
    case "ParameterizedType" @@ (tpt, targs) =>
      AppliedTypeTree(toTypeTree(tpt), toTypeTrees(targs))
    case _ => sys.error(s"Can not translate ${term} to TypeTree")
  }

  def toTypeTrees(term: Term): List[Tree] = term match {
    case "TypeArgs" @@ (Lst(ts@_*)) => (ts map toTypeTree).toList
    case _ => sys.error(s"Can not translate ${term} to TypeTrees")
  }

  def toValDefss(term: Term): List[List[ValDef]] = term match {
    case "Some" @@ ("ParamClause" @@ (Lst(params@_*))) =>
      List((params map toValDef).toList)
    case @@("None") => Nil
    case _ => sys.error(s"Can not transform ${term} to List[List[ValDef]]")
  }

  def toValDefs(term: Term): List[ValDef] = term match {
    case "Bindings" @@ (Lst(bindings@_*)) => (bindings.toList map toValDef)
    case _ => sys.error(s"Can not transform ${term} to List[ValDef]")
  }

  def toValDef(term: Term): ValDef = term match {
    case "Param" @@ (annots, id, typed, rhs) =>
      ValDef(toModifiers(annots), toTermName(id), toTypeTree(typed), toTree(rhs))
    case "Binding" @@ (name, typ) =>
      ValDef(Modifiers(), toTermName(name), toTypeTree(typ), EmptyTree)
    case _ => sys.error(s"Can not transform ${term} to ValDef")
  }

  def toTypeDefs(term: Term): List[TypeDef] = term match {
    case @@("None") => Nil
    case "Id" @@ _ => Nil
    case _ => sys.error(s"Can not transform ${term} to List[TypeDef]")
  }

  def toPackageDef(pkgs: List[Term], topStats: Term): PackageDef = pkgs match {
    case Nil => PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), toTrees(topStats))
    case pkg :: Nil => PackageDef(toRefTree(pkg), toTrees(topStats))
    case pkg :: pkgs => PackageDef(toRefTree(pkg), List(toPackageDef(pkgs, topStats)))
    case _ => sys.error(s"Can not translate ${pkgs} to PackageDef")
  }

  def toRefTree(term: Term): RefTree = term match {
    case "PackageDeclaration" @@ (pkg, _) => toRefTree(pkg)
    case "Id" @@ Str(name) => Ident(name)
    case "QualId" @@ (Lst(id)) => toRefTree(id)
    case "QualId" @@ (Lst(id, ids@_*)) => ids.foldLeft(toRefTree(id)) { (b,a) =>
      Select(b, toTermName(a))
    }
    case _ => sys.error(s"Can not translate ${term} to RefTree")
  }

  def toModifiers(term: Term): Modifiers = term match {
    case Lst() => Modifiers()
    case _ => sys.error(s"Can not translate ${term} to Modifiers")
  }

  def toModifiers(mods: Term, annots: Term) = mods match {
    case @@("None") => Modifiers()
    case _ => sys.error(s"Can not translate ${mods} to Modifiers")
  }

  def toModifiers(mods: Term, annots: Term, accessMods: Term) = {
    // TODO: implement
    Modifiers()
  }

  def toTemplate(body: Term): Option[Tree] = body match {
    case @@("EmptyClassTemplateOpt") => None
    case "TemplateBody" @@ (tplStatSemis) =>
      Some(IUnfinishedTemplate(Nil, None, toTrees(tplStatSemis):_*))
    case "ClassClassTemplateOpt" @@ t => toTemplate(t)
    case "ClassTemplate" @@ (earlyDefs, parents, body) =>
      Some(IUnfinishedTemplate(toTrees(parents), None, toTrees(body):_*))
    case _ => sys.error(s"Can not translate ${body} to Template")
  }

  def toTermName(name: Term): TermName = name match {
    case "Id" @@ t => toTermName(t)
    case Str(name) => name
    case _ => sys.error(s"Can not translate ${name} to TermName")
  }

  def toTypeName(term: Term): TypeName = term match {
    case "Id" @@ t => toTypeName(t)
    case Str(s) => newTypeName(s)
    case _ => sys.error(s"Can not translate ${term} to TypeName")
  }

  object ToFullScalacASTTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case IObjectDef(mods, name, None) => ModuleDef(mods, name, defaultTemplate)
      case IObjectDef(mods, name, Some(t)) => ModuleDef(mods, name, toTemplate(t))
      case IClassDef(mods, name, tparams, None) => ClassDef(mods, name, tparams, defaultTemplate)
      case _ => super.transform(tree)
    }

    def toTemplate(t: Tree) = t match {
      case IUnfinishedTemplate(Nil, None, stats@_*) =>
        Template(List(scalaAnyRefConstr), emptyValDef, (defaultInit +: stats).toList)
      case IUnfinishedTemplate(parents, None, stats@_*) =>
        Template(parents, emptyValDef, (defaultInit +: stats).toList)
      case _ => sys.error(s"Can not finish ${t}")
    }
  }

  class SGLRUnitParser(unit: global.CompilationUnit) {
    def parse(): Tree = {
      val stratego_term = parser.parse(unit.source)
      val wrapped_term = Term(stratego_term)
      val iScalacAST = toTree(wrapped_term)
      val fullyTransformed = ToFullScalacASTTransformer.transform(iScalacAST)
      fullyTransformed
    }
  }

  def defaultTemplate =
    Template(
      List(scalaAnyRefConstr),
      emptyValDef,
      List(defaultInit))

  def superCall =
    Apply(
      gen.mkSuperSelect,
      Nil)

  def defaultInit =
    DefDef(
      NoMods,
      nme.CONSTRUCTOR,
      Nil,
      ListOfNil,
      TypeTree(),
      Block(
        List(superCall),
        Literal(Constant())))

  //def hello_world =
  //  PackageDef(
  //    Ident(nme.EMPTY_PACKAGE_NAME),
  //    List(
  //      ModuleDef(
  //        Modifiers(),
  //        "HelloWorld",
  //        Template(
  //          List(scalaAnyRefConstr),
  //          emptyValDef,
  //          List(defaultInit)))))

  object SGLRParser {
    val tb = new TreeBuilder
    val sglr = new SGLR(tb, scala_tbl)

    def parse(source: SourceFile): IStrategoTerm = {
      sglr.parse(new FileReader(source.file.file), source.file.name, "CompilationUnit") match {
        case v: IStrategoTerm => v
        case unexp => throw new RuntimeException(s"Expected IStrategoTerm, but got ${unexp}")
      }
    }
  }

  object ParseTableManager {
    val ptm = new org.spoofax.jsglr.io.ParseTableManager

    def loadFromStream(stream: InputStream) = ptm.loadFromStream(stream)
    def loadFromFile(file: File) = ptm.loadFromStream(new FileInputStream(file))
  }

  abstract class Term

  case class @@(name: String, children: Term*) extends Term {
    override def toString = {
      val t = children map {_.toString}
      s"${name}(${t.mkString(", ")})"
    }
  }

  case class Lst(elems: Term*) extends Term {
    override def toString = {
      val t = elems map {_.toString}
      s"[${t.mkString(", ")}]"
    }
  }

  case class Str(value: String) extends Term {
    override def toString = "\"" + value + "\""
  }

  object Term {
    def apply(term: IStrategoTerm): Term = term match {
      case appl: StrategoAppl =>
        @@(appl.getName, appl.getAllSubterms() map {Term(_)}: _*)
      case lst: StrategoList =>
        Lst(lst.getAllSubterms() map {Term(_)}: _*)
      case str: StrategoString =>
        Str(str.stringValue())
      case unexp => throw new RuntimeException(s"Unhandled IStrategoTerm: ${unexp}")
    }
  }
}
