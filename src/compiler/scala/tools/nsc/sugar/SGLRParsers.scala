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

  case class IObjectDef(mods: Modifiers, name: TermName) extends Tree

  def toIScalacAST(term: Term): Tree = term match {
    case "CompilationUnit" @@ (Lst(pkgs@_*), Lst(topStats@_*)) => toPackageDef(pkgs, topStats)
    case "TopStatSemi" @@ (topStat, _) => toIScalacAST(topStat)
    case "TopTmplDef" @@ (mods, annots, "Object" @@ ("ObjectDef" @@ (name, body))) =>
      toModuleDef(mods, annots, name, body)
    case _ => sys.error(s"Can not translate term ${term} to scalac AST")
  }

  def toPackageDef(pkgs: Seq[Term], topStats: Seq[Term]) = pkgs match {
    case Nil => PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), (topStats map toIScalacAST).toList)
    case _ => sys.error(s"Can not translate ${pkgs} for PackageDef")
  }

  def toMods(mods: Term, annots: Term) = mods match {
    case @@("None") => Modifiers()
    case _ => sys.error(s"Can not translate ${mods} to Modifiers")
  }

  def toModuleDef(mods: Term, annots: Term, name: Term, body: Term) = body match {
    case @@("EmptyClassTemplateOpt") => IObjectDef(toMods(mods, annots), toTermName(name))
    case _ => sys.error(s"Can not translate ${body} for ModuleDef")
  }

  def toTermName(name: Term) = name match {
    case "Id" @@ Str(name) => name
    case _ => sys.error(s"Can not translate ${name} to TermName")
  }

  def toTemplate(body: Term) = body match {
    case @@("EmptyClassTemplateOpt") =>
  }

  object ToFullScalacASTTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case IObjectDef(mods, name) => ModuleDef(mods, name, defaultTemplate)
      case _ => super.transform(tree)
    }
  }

  class SGLRUnitParser(unit: global.CompilationUnit) {
    def parse(): Tree = {
      val stratego_term = parser.parse(unit.source)
      val wrapped_term = Term(stratego_term)
      val iScalacAST = toIScalacAST(wrapped_term)
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
