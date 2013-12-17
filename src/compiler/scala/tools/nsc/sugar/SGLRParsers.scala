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

trait SGLRParsers {
  val global : Global
  import global._
  import treeBuilder.{global => _, _}

  val scala_tbl_stream =
    getClass.getResourceAsStream("/scala/tools/nsc/sugar/Scala.tbl")
  val scala_tbl = ParseTableManager.loadFromStream(scala_tbl_stream)
  val parser = SGLRParser

  case class IUnfinishedTemplate(parents: Seq[Tree], self: Option[ValDef], body: Tree*) extends Tree
  case class IObjectDef(mods: Modifiers, name: TermName, tpl: Option[IUnfinishedTemplate]) extends Tree

  def toTree(term: Term): Tree = term match {
    case "CompilationUnit" @@ (pkgs, topStats) => toPackageDef(pkgs, topStats)

    case "TopStatSemi" @@ (topStat, _) => toTree(topStat)

    case "TemplateStatSemi" @@ (stat, _) => toTree(stat)

    case "TopTmplDef" @@ (mods, annots, "Object" @@ ("ObjectDef" @@ (name, body))) =>
      IObjectDef(toModifiers(mods, annots), toTermName(name), toTemplate(body))

    case "DefTemplateStat" @@ (mods, annots, "FunDefDef" @@ funDef) =>
      toDefDef(toModifiers(mods, annots), funDef)

    case "Some" @@ (t) => toTree(t)

    case @@("None") => EmptyTree

    case "ParamTyped" @@ (t) => toTree(t)

    case "ParameterizedType" @@ (tpt, targs) =>
      AppliedTypeTree(toTree(tpt), toTrees(targs))

    case "Type" @@ ("Id" @@ (Str(name))) => Ident(newTypeName(name))

    case "Block" @@ (t) => Block(toTrees(t):_*)

    case "BlockStatSemi" @@ (t, _) => toTree(t)

    case "AppExpr" @@ (fun, args) => Apply(toTree(fun), toTrees(args))

    case "Id" @@ Str(name) => Ident(name)

    case "String" @@ Str(s) => Literal(Constant(s))

    case _ => sys.error(s"Can not translate term ${term} to Tree")
  }

  def toTrees(term: Term): List[Tree] = term match {
    case "TypeArgs" @@ (Lst(ts@_*)) => (ts map toTree).toList
    case Lst(ts@_*) => (ts map toTree).toList
    case "ArgumentExprs" @@ t => toTrees(t)
    case "Some" @@ t => toTrees(t)
    case @@("None") => Nil
    case "Exprs" @@ t => toTrees(t)
    case _ => sys.error(s"Can not transform ${term} to List[Tree]")
  }

  def toDefDef(mods: Modifiers, funDef: Term): DefDef = funDef match {
    case "ProcDef" @@ ("FunSig" @@ ("Id" @@ Str(name), tParams, vParams), body) =>
      DefDef(mods, newTermName(name), toTypeDefs(tParams), toValDefss(vParams),
             TypeTree(), toTree(body))
  }

  def toValDefss(term: Term): List[List[ValDef]] = term match {
    case "Some" @@ ("ParamClause" @@ (Lst(params@_*))) =>
      List((params map toValDef).toList)
    case @@("None") => Nil
    case _ => sys.error(s"Can not transform ${term} to List[List[ValDef]]")
  }

  def toValDef(term: Term): ValDef = term match {
    case "Param" @@ (annots, id, typed, rhs) =>
      ValDef(toModifiers(annots), toTermName(id), toTree(typed), toTree(rhs))
  }

  def toTypeDefs(term: Term): List[TypeDef] = term match {
    case @@("None") => Nil
    case _ => sys.error(s"Can not transform ${term} to List[TypeDef]")
  }

  def toPackageDef(pkgs: Term, topStats: Term): PackageDef = pkgs match {
    case Lst() => PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), toTrees(topStats))
    case Lst("PackageDeclaration" @@ (id, _)) =>
      PackageDef(toRefTree(id), toTrees(topStats))
    case _ => sys.error(s"Can not translate ${pkgs} to PackageDef")
  }

  def toRefTree(term: Term): RefTree = term match {
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

  def toTemplate(body: Term) = body match {
    case @@("EmptyClassTemplateOpt") => None
    case "TemplateBody" @@ (Lst(tplStatSemis@_*)) =>
      Some(IUnfinishedTemplate(Nil, None, tplStatSemis map toTree:_*))
    case _ => sys.error(s"Can not translate ${body} to Template")
  }

  def toTermName(name: Term) = name match {
    case "Id" @@ Str(name) => name
    case _ => sys.error(s"Can not translate ${name} to TermName")
  }

  object ToFullScalacASTTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case IObjectDef(mods, name, None) => ModuleDef(mods, name, defaultTemplate)
      case IObjectDef(mods, name, Some(t)) => ModuleDef(mods, name, toTemplate(t))
      case _ => super.transform(tree)
    }

    def toTemplate(v: IUnfinishedTemplate) = v match {
      case IUnfinishedTemplate(Nil, None, stats@_*) =>
        Template(List(scalaAnyRefConstr), emptyValDef, (defaultInit +: stats).toList)
      case _ => sys.error(s"Can not finish ${v}")
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
