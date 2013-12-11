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
  val scala_tbl_file = new File("../../../lib/extra/Scala.tbl")
  val scala_tbl = ParseTableManager.loadFromFile(scala_tbl_file)
  val parser = SGLRParser

  def toScalacAST(term: StrategoTerm): Tree = term match {
    case EmptyPackage(stats) =>
      PackageDef(
        Ident(nme.EMPTY_PACKAGE_NAME),
        stats map toScalacAST)
    case sugar.ModuleDef(mods, name, tpl) =>
      ModuleDef(
        toScalacMods(mods),
        name,
        toScalacTemplate(tpl))
  }

  def toScalacMods(mods: sugar.Modifiers) = mods match {
    case NoModifiers() => Modifiers()
  }

  def toScalacTemplate(tpl: sugar.Template) = tpl match {
    case EmptyTemplate() =>
      Template(
        List(scalaAnyRefConstr),
        emptyValDef,
        List(defaultInit))
  }

  class SGLRUnitParser(unit: global.CompilationUnit) {
    def parse(): Tree = {
      val strategoTerm = parser.parse(unit.source)
      val wrappedTerm = StrategoTerm(strategoTerm)
      val parseTree = ParseTree(wrappedTerm)
      val normalizedTree = NormalizedTree(parseTree)
      val scalacAST = toScalacAST(normalizedTree)
      scalacAST
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

    def parse(source: SourceFile): IStrategoTerm =
      sglr.parse(
             new FileReader(source.file.file),
             source.file.name,
             "CompilationUnit") match {
        case v: IStrategoTerm => v
        case unexp => sys.error(s"Expected IStrategoTerm, but got ${unexp}")
      }
  }

  object ParseTableManager {
    val ptm = new org.spoofax.jsglr.io.ParseTableManager

    def loadFromFile(file: File) = ptm.loadFromStream(new FileInputStream(file))
  }
}
