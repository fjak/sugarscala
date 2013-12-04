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

trait SGLRParsers {
  val global : Global
  import global._
  import treeBuilder.{global => _, _}

  // TODO: figure out better way to get the parse table file
  val scala_tbl_file = new File("../../../lib/Scala.tbl")
  val scala_tbl = ParseTableManager.loadFromFile(scala_tbl_file)
  val parser = SGLRParser

  def toScalacAST(term: Term): Tree = {
    hello_world
  }

  class SGLRUnitParser(unit: global.CompilationUnit) {
    def parse(): Tree = {
      val stratego_term = parser.parse(unit.source)
      val wrapped_term = Term(stratego_term)
      toScalacAST(wrapped_term)
    }
  }

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

  def hello_world =
    PackageDef(
      Ident(nme.EMPTY_PACKAGE_NAME),
      List(
        ModuleDef(
          Modifiers(),
          "HelloWorld",
          Template(
            List(scalaAnyRefConstr),
            emptyValDef,
            List(defaultInit)))))

  object SGLRParser {
    val tb = new TreeBuilder
    val sglr = new SGLR(tb, scala_tbl)

    def parse(source: SourceFile): IStrategoTerm = {
      sglr.parse(new FileReader(source.file.file), source.file.name) match {
        case v: IStrategoTerm => v
        case unexp => throw new RuntimeException(s"Expected IStrategoTerm, but got ${unexp}")
      }
    }
  }

  object ParseTableManager {
    val ptm = new org.spoofax.jsglr.io.ParseTableManager

    def loadFromFile(file: File) = ptm.loadFromStream(new FileInputStream(file))
  }


  trait Term

  case class Appl(name: String, children: Term*) extends Term {
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
        Appl(appl.getName, appl.getAllSubterms() map {Term(_)}: _*)
      case lst: StrategoList =>
        Lst(lst.getAllSubterms() map {Term(_)}: _*)
      case str: StrategoString =>
        Str(str.stringValue())
      case unexp => throw new RuntimeException(s"Unhandled IStrategoTerm: ${unexp}")
    }
  }
}
