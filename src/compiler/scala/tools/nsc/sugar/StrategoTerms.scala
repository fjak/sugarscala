package scala.tools.nsc.sugar

import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.terms.StrategoAppl
import org.spoofax.terms.StrategoList
import org.spoofax.terms.StrategoString

trait StrategoTerms {
  trait StrategoTerm

  case class Appl(name: String, children: StrategoTerm*) extends StrategoTerm {
    override def toString = {
      val t = children map {_.toString}
      s"${name}(${t.mkString(", ")})"
    }
  }

  case class Lst(elems: StrategoTerm*) extends StrategoTerm {
    override def toString = {
      val t = elems map {_.toString}
      s"[${t.mkString(", ")}]"
    }
  }

  case class Str(value: String) extends StrategoTerm {
    override def toString = "\"" + value + "\""
  }

  object StrategoTerm {
    def apply(term: IStrategoTerm): StrategoTerm = term match {
      case appl: StrategoAppl =>
        Appl(appl.getName, appl.getAllSubterms() map {StrategoTerm(_)}: _*)
      case lst: StrategoList =>
        Lst(lst.getAllSubterms() map {StrategoTerm(_)}: _*)
      case str: StrategoString =>
        Str(str.stringValue())
      case unexp => sys.error(s"Unhandled IStrategoTerm: ${unexp}")
    }
  }
}
