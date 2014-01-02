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
import scala.reflect.internal.{ModifierFlags => Flags}
import org.apache.commons.lang.StringEscapeUtils
import org.spoofax.jsglr.client.imploder.ImploderAttachment
import scala.reflect.internal.util.OffsetPosition

trait SGLRParsers {
  val global : Global
  import global._
  import treeBuilder.{global => _, _}

  def unescape(s: String): String = {
    s.drop(1).dropRight(1).replaceAllLiterally("""\"""", "\"")
  }

  def toChar(s: String): Char = StringEscapeUtils.unescapeJava(s.drop(1).dropRight(1)).charAt(0)

  val scala_tbl_stream =
    getClass.getResourceAsStream("/scala/tools/nsc/sugar/Scala.tbl")
  val scala_tbl = ParseTableManager.loadFromStream(scala_tbl_stream)
  val parser = SGLRParser

  case class IUnfinishedTemplate(parents: List[Tree], attrss: List[List[Tree]], self: ValDef, body: Tree*) extends Tree
  case class IObjectDef(mods: Modifiers, name: TermName, tpl: Option[Tree]) extends Tree
  case class IClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], accessMods: Modifiers, vparamss: List[List[ValDef]], impl: Option[Tree]) extends Tree

  def isInterface(mods: Modifiers, body: List[Tree]): Boolean =
    mods.isTrait && (body forall treeInfo.isInterfaceMember)

  def toTree(term: Term): Tree = term match {
    // --- Top Level Statements ---
    case "CompilationUnit" @@ (Lst(pkgs@_*), topStats) => toPackageDef(pkgs.toList, topStats)

    case "TopStatSemi" @@ (topStat, _) => toTree(topStat)

    case "TopTmplDef" @@ (mods, annots, "Object" @@ ("ObjectDef" @@ (name, body))) =>
      IObjectDef(toModifiers(mods, annots), toTermName(name), toTemplate(body))

    case "TopTmplDef" @@
           (annots, mods, "Class" @@
              ("ClassDef" @@ (morphism, constrAnnots, accessMods, classParamClauses, tplOpt))) =>
      IClassDef(toModifiers(mods, annots), toTypeName(morphism), toTypeDefs(morphism), toModifiers(accessMods), toValDefss(classParamClauses), toTemplate(tplOpt))

    case "TopTmplDef" @@
           (annots, mods, "Trait" @@
              ("TraitDef" @@ (id, typeParams, tplOpt))) =>
      IClassDef(toModifiers(mods, annots) | Flags.TRAIT | Flags.ABSTRACT, toTypeName(id), toTypeDefs(typeParams), Modifiers() | Flags.TRAIT, ListOfNil, toTemplate(tplOpt))

    // --- Template Level Statements ---
    case "TemplateStatSemi" @@ (stat, _) => toTree(stat)

    case "DefTemplateStat" @@ (annots, mods, "FunDefDef" @@ funDef) =>
      toDefDef(toModifiers(mods, annots), funDef)

    case "DclTemplateStat" @@ (annots, mods, "FunDclDcl" @@ funDcl) =>
      toDefDef(toModifiers(mods, annots), funDcl)

    case "DclTemplateStat" @@ (annots, mods, "ValDclDcl" @@ valDcl) =>
      toValDef(valDcl, toModifiers(mods, annots))

    case "DclTemplateStat" @@ (annots, mods, "VarDclDcl" @@ valDcl) =>
      toValDef(valDcl, toModifiers(mods, annots))

    case "DefTemplateStat" @@
           (annots, mods, "Class" @@
              ("ClassDef" @@ (morphism, constrAnnots, accessMods, classParamClauses, tplOpt))) =>
      IClassDef(toModifiers(mods, annots), toTypeName(morphism), toTypeDefs(morphism), toModifiers(accessMods), toValDefss(classParamClauses), toTemplate(tplOpt))

    case "DefTemplateStat" @@ (annots, mods, "Object" @@ ("ObjectDef" @@ (name, body))) =>
      IObjectDef(toModifiers(mods, annots), toTermName(name), toTemplate(body))

    case "DefTemplateStat" @@
           (annots, mods, "TypeDefDef" @@
              ("TypeDef" @@ (id, tpc, rhs))) =>
      TypeDef(toModifiers(mods, annots), toTypeName(id), toTypeDefs(tpc), toTypeTree(rhs))

    case "DefTemplateStat" @@ (annots, mods, "ValPatDef" @@ ("PatDef" @@ (Lst(name), typ, expr))) =>
      ValDef(toModifiers(mods, annots), toTermName(name), toTypeTree(typ), toExpr(expr))

    case "DefTemplateStat" @@ (annots, mods, "VarPatDef" @@ ("PatDef" @@ (Lst(name), typ, expr))) =>
      ValDef(toModifiers(mods, annots) | Flags.MUTABLE, toTermName(name), toTypeTree(typ), toExpr(expr))

    case "DefTemplateStat" @@ (annots, mods, "VarPatDef" @@ ("WildcardVarDef" @@ (Lst(name), typ))) =>
      ValDef(toModifiers(mods, annots) | Flags.MUTABLE | Flags.DEFAULTINIT, toTermName(name), toTypeTree(typ), EmptyTree)

    case "ExprTemplateStat" @@ t => toExpr(t)


    // --- Block Level Statements ---
    case "BlockStatSemi" @@ (t, _) => toTree(t)

    case "DefBlockStat" @@ (annots, "ValPatDef" @@ ("PatDef" @@ (Lst(name), typ, expr))) =>
      ValDef(toModifiers(annots), toTermName(name), toTypeTree(typ), toExpr(expr))

    case "DefBlockStat" @@ (annots, "VarPatDef" @@ ("PatDef" @@ (Lst(name), typ, expr))) =>
      ValDef(toModifiers(annots) | Flags.MUTABLE, toTermName(name), toTypeTree(typ), toExpr(expr))



    // --- Imports ---
    case t @ @@("ImportExpr", _*) => toImport(t)

    case t @ @@("WildcardImportExpr", _*) => toImport(t)

    case t @ @@("SelectorsImportExpr", _*) => toImport(t)


    // --- Some and None ---
    case "Some" @@ (t) => toTree(t)

    case @@("None") => EmptyTree

    case _ => toExpr(term)
  }

  def toTrees(term: Term): List[Tree] = term match {
    case Lst() => Nil
    case Lst(t, ts@_*) => t match {
      case "TopStatSemi" @@ (imprt @ @@("Import", _*), _) => toTrees(Lst((imprt +: ts):_*))
      case "Import" @@ (imports) => toTrees(imports) ::: toTrees(Lst(ts:_*))
      case _ => toTree(t) :: toTrees(Lst(ts:_*))
    }
    case "ArgumentExprs" @@ t => toArgs(t)
    case "Some" @@ t => toTrees(t)
    case @@("None") => Nil
    case "Exprs" @@ t => toTrees(t)
    case "TemplateBody" @@ tplStatSemis => toTrees(tplStatSemis)
    case "ClassTemplate" @@ (earlyDefs, parents, body) => toTrees(body)
    case _ => sys.error(s"Can not transform ${term} to List[Tree]")
  }

  def toTreess(term: Term): List[List[Tree]] = term match {
    case @@("None") => ListOfNil
    case "Some" @@ t => toTreess(t)
    case "ClassParents" @@ ("Constr" @@ (_, args), _) => toTreess(args)
    case "TraitParents" @@ (_, _) => ListOfNil
    case "ArgumentExprs" @@ t => List(toTrees(t))
    case "ArgumentExprsSeq" @@ (hd, tl) => toTrees(hd) :: toTreess(tl)
    case "ClassTemplate" @@ (_, classParents, _) => toTreess(classParents)
    case _ => sys.error(s"Can not translate ${term} to List[List[Tree]]")
  }

  var placeholderParams: List[ValDef] = Nil

  def toExpr(term: Term): Tree = {
    var savedPlaceholderParams = placeholderParams
    placeholderParams = List()
    var res = toExpr0(term)
    if (!placeholderParams.isEmpty) {
      res = atPos(res.pos){ Function(placeholderParams.reverse, res) }
      placeholderParams = List()
    }
    placeholderParams = placeholderParams ::: savedPlaceholderParams
    res
  }

  def toExpr0(term: Term): Tree = term match {
    case @@("WildcardExpr") => {
      val pname = freshName("x$")
      val id = Ident(pname)
      val param = makeSyntheticParam(pname)
      placeholderParams = param :: placeholderParams
      id
    }

    // --- Identifiers ---
    case "Id" @@ Str(name) => Ident(name)

    case "StableId" @@ (qid, select) => Select(toExpr0(qid), toTermName(select))

    case Lst(t, ts@_*) => {
      if (ts.nonEmpty && ts.head == @@("This")) {
        val base: Tree = This(toTypeName(t))
        ts.tail.foldLeft(base) {(b,a) => Select(b, toTermName(a))}
      } else ts.foldLeft(toExpr0(t)) {(b,a) => Select(b, toTermName(a))}
    }

    // --- Expressions ---
    case "BlockExpr" @@ t => toExpr(t)

    case "Block" @@ t => makeBlock(toTrees(t))
    case "Block" @@ (stats, sre) => makeBlock(toTrees(stats) :+ toExpr(sre))

    case "SimpleResultExpr" @@ (id, typ, body) =>
      Function(List(ValDef(NoMods | Flags.PARAM, toTermName(id), toTypeTree(typ), EmptyTree)), toTree(body))

    case Str(name) => Ident(name)

    case "Path" @@ l => toExpr0(l)

    case "Char" @@ Str(s) => Literal(Constant(toChar(s)))

    case "String" @@ Str(s) => Literal(Constant(unescape(s)))

    case "Int" @@ Str(s) => Literal(Constant(s.toInt))

    case @@("False") => Literal(Constant(false))

    case @@("True") => Literal(Constant(true))

    case @@("Null") => Literal(Constant(null))

    case @@("This") => This(nme.EMPTY.toTypeName)

    case "AppExpr" @@ (fun, args) => Apply(toExpr0(fun), toTrees(args))

    case "PrefixExpr" @@ (op, arg) =>
      Select(toExpr0(arg), toTermName(op).encode.prepend("unary_"))

    case "InfixExpr" @@ (lhs, op, rhs) =>
      Apply(Select(toExpr0(lhs), toTermName(op).encode), List(toExpr0(rhs)))

    case "PostfixExpr" @@ (arg, op) => Select(toExpr0(arg), toTermName(op).encode)

    case "DesignatorExpr" @@ (t, sel) => Select(toExpr0(t), toTermName(sel))

    case "DesignatorAssignmentExpr" @@ (expr, id, rhs) =>
      Assign(Select(toExpr0(expr), toTermName(id)), toExpr0(rhs))

    case "TupleExpr" @@ Lst(terms@_*) =>
      makeTupleTerm(terms.toList map toExpr0, true)

    case "FunExpr" @@ (bindings, body) => Function(toValDefs(bindings), toExpr0(body))

    case "IdFunExpr" @@ (id, body) =>
      Function(List(ValDef(NoMods | Flags.PARAM, toTermName(id), TypeTree(), EmptyTree)), toExpr0(body))

    case "AssignmentExpr" @@ (lhs, rhs) => Assign(toExpr0(lhs), toExpr0(rhs))

    case "Assignment" @@ expr => toExpr0(expr)

    case "IfExpr" @@ (cond, then) => If(toExpr(cond), toExpr0(then), Literal(Constant()))

    case "IfElseExpr" @@ (cond, then, els) => If(toExpr(cond), toExpr0(then), toExpr0(els))

    case "MatchExpr" @@ (t, clauses) => Match(toExpr0(t), toCaseDefs(clauses))

    case t @ "NewClassExpr" @@ tpl =>
      makeNew(toTypeTrees(tpl), emptyValDef, toTrees(tpl), toTreess(tpl), t.pos, tpl.pos)

    case "TypeApplication" @@ (expr, typeArgs) => TypeApply(toExpr0(expr), toTypeTrees(typeArgs))

    case "WhileExpr" @@ (cond, body) => makeWhile(-1, toExpr(cond), toExpr0(body))

    case "SelfInvocation" @@ argExprsSeq => {
      val argss = toTreess(argExprsSeq)
      argss match {
        case Nil => Apply(Ident(nme.CONSTRUCTOR), Nil)
        case List(Nil) => Apply(Ident(nme.CONSTRUCTOR), Nil)
        case hd :: tl => {
          val base = Apply(Ident(nme.CONSTRUCTOR), hd)
          tl.foldLeft(base) {(b,a) => Apply(b, a)}
        }
      }
    }

    case "ThrowExpr" @@ e => Throw(toExpr0(e))

    case _ => sys.error(s"Can not translate ${term} to expr")
  }

  def toArg(term: Term): Tree = term match {
    case "AssignmentExpr" @@ (lhs, rhs) => AssignOrNamedArg(toTree(lhs), toTree(rhs))
    case _ => toTree(term)
  }

  def toArgs(term: Term): List[Tree] = term match {
    case "Some" @@ t => toArgs(t)
    case @@("None") => toTrees(term)
    case "Exprs" @@ t => toArgs(t)
    case Lst(exprs@_*) => exprs.toList map toArg
    case "ClassTemplate" @@ (_, classParents, _) => toArgs(classParents)
    case "ClassParents" @@ (constr, _) => toArgs(constr)
    case "Constr" @@ (_, @@("None")) => Nil
    case _ => sys.error(s"Can not transform ${term} to arguments (List[Tree])")
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
      val termName = newTermName(name)
      List(ImportSelector(termName, -1, termName, -1))
    }
    case "ImportSelectors" @@ Lst(t@_*) => (t map toImportSelector).toList
    case _ => sys.error(s"Can not transform ${term} to ImportSelectors")
  }

  def toImportSelector(term: Term): ImportSelector = term match {
    case "ImportSelector" @@ t => {
      val name = toTermName(t)
      ImportSelector(name, -1, name, -1)
    }
    case "MappedImportSelector" @@ (n, m) =>
      ImportSelector(toTermName(n), -1, toTermName(m), -1)
    case _ => sys.error(s"Can not transform ${term} to ImportSelector")
  }

  def toDefDef(mods: Modifiers, funDef: Term): DefDef = funDef match {
    case "ProcDef" @@ ("FunSig" @@ (id, tParams, vParams), body) =>
      DefDef(mods, toTermName(id), toTypeDefs(tParams), toValDefss(vParams),
             scalaUnitConstr, toTree(body))
    case "ProcDcl" @@ ("FunSig" @@ (id, tParams, vParams)) =>
      DefDef(mods | Flags.DEFERRED, toTermName(id), toTypeDefs(tParams),
             toValDefss(vParams), scalaUnitConstr, EmptyTree)
    case "FunDef" @@ ("FunSig" @@ (id, tParams, vParams), retType, body) =>
      DefDef(mods, toTermName(id), toTypeDefs(tParams), toValDefss(vParams),
             toTypeTree(retType), toTree(body))
    case "FunDcl" @@ ("FunSig" @@ (id, tParams, vParams), retType) =>
      DefDef(mods | Flags.DEFERRED, toTermName(id), toTypeDefs(tParams),
             toValDefss(vParams), toTypeTree(retType), EmptyTree)
    case t @ "ThisExprFunDef" @@ (vParams, expr) =>
      atPos(t.pos) {
        DefDef(mods, nme.CONSTRUCTOR, Nil, toValDefss(vParams), TypeTree(),
          Block(List(toTree(expr)), Literal(Constant())))
      }
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
    case "Constr" @@ (typ, args) => toTypeTree(typ)
    case "WithAnnotType" @@ t => toTypeTree(t)
    case "FunctionType" @@ (argtpes, restpe) =>
      makeFunctionTypeTree(toTypeTrees(argtpes), toTypeTree(restpe))
    case "TupleType" @@ l => toTypeTree(l)
    case Lst(types@_*) => makeTupleType(types.toList map toTypeTree, true)
    case "StableId" @@ (l, sel) => Select(toTree(l), toTypeName(sel))
    case "ClassTemplate" @@ (_, classParents, _) => toTypeTree(classParents)
    case "ClassParents" @@ (constr, _) => toTypeTree(constr)
    case "SingletonType" @@ t => SingletonTypeTree(toTree(t))
    case "UpperBoundType" @@ t => toTypeTree(t)
    case "LowerBoundType" @@ t => toTypeTree(t)
    case "CompoundType" @@ (typ, withs, refinement) =>
      CompoundTypeTree(Template(toTypeTree(typ) :: toTypeTrees(withs), emptyValDef, Nil))
    case "With" @@ t => toTypeTree(t)
    case _ => sys.error(s"Can not translate ${term} to TypeTree")
  }

  def toTypeTrees(term: Term): List[Tree] = term match {
    case "TypeArgs" @@ (Lst(ts@_*)) => (ts map toTypeTree).toList
    case "ClassParents" @@ (parent, withs) => toTypeTree(parent) :: toTypeTrees(withs)
    case "TraitParents" @@ (parent, withs) => toTypeTree(parent) :: toTypeTrees(withs)
    case Lst(withs@_*) => withs.toList map toTypeTree
    case t @ "Type" @@ _ => List(toTypeTree(t))
    case "FunctionArgType" @@ l => toTypeTrees(l)
    case "ClassTemplate" @@ (earlyDefs, parents, body) => toTypeTrees(parents)
    case _ => List(toTypeTree(term))
  }

  def toValDef(term: Term, modifiers: Modifiers = Modifiers()): ValDef = term match {
    case "ClassParam" @@ (annots, id, typed, rhs) =>
      ValDef(toModifiers(annots) | Flags.PrivateLocal | Flags.PARAM | Flags.PARAMACCESSOR | modifiers.flags, toTermName(id), toTypeTree(typed), toTree(rhs))
    case "ValClassParam" @@ (annots, mods, id, typed, rhs) =>
      ValDef(toModifiers(mods, annots) | Flags.PARAM | Flags.PARAMACCESSOR | modifiers.flags, toTermName(id), toTypeTree(typed), toTree(rhs))
    case "VarClassParam" @@ (annots, mods, id, typed, rhs) =>
      ValDef(toModifiers(mods, annots) | Flags.PARAM | Flags.PARAMACCESSOR | Flags.MUTABLE | modifiers.flags, toTermName(id), toTypeTree(typed), toTree(rhs))
    case "Param" @@ (annots, id, typed, rhs) => {
      val flags = rhs match {
        case "Some" @@ ("Assignment" @@ e) => Flags.DEFAULTPARAM
        case _ => NoMods.flags
      }
      ValDef(toModifiers(annots) | modifiers.flags | flags, toTermName(id), toTypeTree(typed), toTree(rhs))
    }
    case "Binding" @@ (name, typ) =>
      ValDef(Modifiers() | modifiers.flags, toTermName(name), toTypeTree(typ), EmptyTree)
    case "ValDcl" @@ (Lst(name), typ) =>
      ValDef(modifiers | Flags.DEFERRED, toTermName(name), toTypeTree(typ), EmptyTree)
    case "VarDcl" @@ (Lst(name), typ) =>
      ValDef(modifiers | Flags.MUTABLE | Flags.DEFERRED, toTermName(name), toTypeTree(typ), EmptyTree)
    case _ => sys.error(s"Can not transform ${term} to ValDef")
  }

  def toValDefs(term: Term, mods: Modifiers = Modifiers()): List[ValDef] = term match {
    case Lst(params@_*) => params.toList map { t => toValDef(t, mods) }
    case "ClassParamClause" @@ lst => toValDefs(lst)
    case "Bindings" @@ l => toValDefs(l, mods)
    case _ => sys.error(s"Can not transform ${term} to List[ValDef]")
  }

  def toValDefss(term: Term): List[List[ValDef]] = term match {
    case @@("None") => Nil
    case "Some" @@ t => toValDefss(t)
    case "ParamClause" @@ t => List(toValDefs(t))
    case "ImplicitParamClause" @@ t => List(toValDefs(t, Modifiers() | Flags.IMPLICIT))
    case "ParamClauses" @@ ("ParamClause" @@ t, cls) => toValDefs(t) :: toValDefss(cls)
    case "ClassParamClause" @@ t => List(toValDefs(t))
    case "ClassParamClauses" @@ (hd, tl) => toValDefs(hd) :: toValDefss(tl)
    case _ => sys.error(s"Can not transform ${term} to List[List[ValDef]]")
  }

  def toTypeDef(term: Term): TypeDef = term match {
    case "VariantTypeParam" @@ (Lst(), typeParam) => toTypeDef(typeParam)
    case "PlusVariantTypeParam" @@ (Lst(), typeParam) => {
      val td = toTypeDef(typeParam)
      td.copy(mods = td.mods | Flags.COVARIANT)
    }
    case "TypeParam" @@ (id, tpc, lbt, ubt, Lst(), Lst()) =>
      TypeDef(Modifiers() | Flags.PARAM, toTypeName(id), toTypeDefs(tpc), toTypeBoundsTree(lbt, ubt))
    case _ => sys.error(s"Can not transform ${term} to TypeDef")
  }

  def toTypeDefs(term: Term): List[TypeDef] = term match {
    case @@("None") => Nil
    case "Some" @@ t => toTypeDefs(t)
    case "Id" @@ _ => Nil
    case "TypeParamClause" @@ lst => toTypeDefs(lst)
    case Lst(defs@_*) => defs.toList map toTypeDef
    case "Polymorph" @@ (_, defs) => toTypeDefs(defs)
    case _ => sys.error(s"Can not transform ${term} to List[TypeDef]")
  }

  def toTypeBoundsTree(lbt: Term, ubt: Term): TypeBoundsTree = {
    val lb = lbt match {
      case @@("None") => rootScalaDot(tpnme.Nothing)
      case "Some" @@ t => toTypeTree(t)
    }
    val ub = ubt match {
      case @@("None") => rootScalaDot(tpnme.Any)
      case "Some" @@ t => toTypeTree(t)
    }
    TypeBoundsTree(lb, ub)
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
    case Lst(mod, mods@_*) => mods.foldLeft(toModifiers(mod)) {(b, a) => b | toModifiers(a).flags}
    case @@("None") => Modifiers()
    case "Some" @@ t => toModifiers(t)
    case @@("OverrideModifier") => Modifiers() | Flags.OVERRIDE
    case @@("ImplicitModifier") => Modifiers() | Flags.IMPLICIT
    case @@("AbstractModifier") => Modifiers() | Flags.ABSTRACT
    case "PrivateModifier" @@ (@@("None")) =>
      Modifiers() | Flags.PRIVATE
    case "PrivateModifier" @@ ("Some" @@ ("AccessQualifier" @@ id)) =>
      Modifiers(NoMods.flags, toTypeName(id))
    case "PrivateModifier" @@ ("Some" @@ (@@("ThisQualifier"))) =>
      Modifiers(NoMods.flags) | Flags.PrivateLocal
    case "ProtectedModifier" @@ (@@("None")) =>
      Modifiers() | Flags.PROTECTED
    case "ProtectedModifier" @@ ("Some" @@ ("AccessQualifier" @@ id)) =>
      Modifiers(NoMods.flags, toTypeName(id)) | Flags.PROTECTED
    case "ProtectedModifier" @@ ("Some" @@ (@@("ThisQualifier"))) =>
      Modifiers(NoMods.flags) | Flags.ProtectedLocal
    case @@("LazyModifier") => Modifiers() | Flags.LAZY
    case _ => sys.error(s"Can not translate ${term} to Modifiers")
  }

  def toModifiers(mods: Term, annots: Term): Modifiers = toModifiers(mods)

  def toCaseDef(term: Term): CaseDef = term match {
    case "CaseClause" @@ (pat, @@("None"), blk) =>
      CaseDef(toPatternTree(pat), EmptyTree, toTree(blk))
    case _ => sys.error(s"Can not translate ${term} to CaseDef")
  }

  def toCaseDefs(term: Term): List[CaseDef] = term match {
    case Lst(clauses@_*) => clauses.toList map toCaseDef
    case _ => sys.error(s"Can not translate ${term} to List[CaseDef]")
  }

  def toPatternTree(term: Term): Tree = term match {
    case "LiteralPattern" @@ lit => toTree(lit)
    case @@("WildcardPattern") => Ident(nme.WILDCARD)
    case _ => sys.error(s"Can not translate ${term} to Pattern")
  }

  def toTemplate(body: Term): Option[Tree] = body match {
    case @@("EmptyClassTemplateOpt") => None
    case @@("EmptyTraitTemplateOpt") => None
    case "TemplateBody" @@ (tplStatSemis) =>
      Some(IUnfinishedTemplate(Nil, ListOfNil, emptyValDef, toTrees(tplStatSemis):_*))
    case "ClassClassTemplateOpt" @@ t => toTemplate(t)
    case "TraitTraitTemplateOpt" @@ t => toTemplate(t)
    case "ClassTemplate" @@ (earlyDefs, parents, body) =>
      Some(IUnfinishedTemplate(toTypeTrees(parents), toTreess(parents), emptyValDef, toTrees(body):_*))
    case "TraitTemplate" @@ (earlyDefs, parents, body) =>
      Some(IUnfinishedTemplate(toTypeTrees(parents), toTreess(parents), emptyValDef, toTrees(body):_*))
    case _ => sys.error(s"Can not translate ${body} to Template")
  }

  def toTermName(name: Term): TermName = name match {
    case "Id" @@ t => toTermName(t)
    case Str(name) => newTermName(name).encode
    case _ => sys.error(s"Can not translate ${name} to TermName")
  }

  def toTypeName(term: Term): TypeName = term match {
    case "Id" @@ t => toTypeName(t)
    case Str(s) => newTypeName(s)
    case "Polymorph" @@ (id, _) => toTypeName(id)
    case _ => sys.error(s"Can not translate ${term} to TypeName")
  }

  object ToFullScalacASTTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      // The EmptyTree as rest of a template is lost on the recursive call.
      // This will create a difference in the resulting AST, when providing an
      // explicitly empty body, like `object Foo { }`. At least this has no influence on
      // the resulting bytecode.
      // I currently have no idea how to circumvent this. Trying to introduce
      // an extra case like `EmptyTree => EmptyTree` does not help.
      case IObjectDef(mods, name, impl) => {
        val tpl = mkTemplate(impl, NoMods, ListOfNil)
        ModuleDef(mods, name, transform(tpl).asInstanceOf[Template])
      }
      case IClassDef(mods, name, tparams, aMods, vparamss, impl) => {
        val tpl = mkTemplate(impl, aMods, vparamss)
        val mods1 = if (isInterface(mods, tpl.body)) mods | Flags.INTERFACE else mods
        ClassDef(mods1, name, tparams, transform(tpl).asInstanceOf[Template])
      }
      case _ => super.transform(tree)
    }

    def mkTemplate(impl: Option[Tree], aMods: Modifiers, vparamss: List[List[ValDef]]): Template = impl match {
      case None =>
        Template(List(scalaAnyRefConstr), emptyValDef, aMods, vparamss, ListOfNil, Nil, NoPosition)
      case Some(IUnfinishedTemplate(parents, attrs, selfVal, stats@_*)) => {
        val parents0 = if (parents.isEmpty) List(scalaAnyRefConstr) else parents
        val stats0 = if (stats.isEmpty) EmptyTree.asList else stats.toList
        Template(parents0, selfVal, aMods, vparamss, attrs, stats0, NoPosition)
      }
      case _ => sys.error(s"Can not make template with impl: ${impl}, aMods: ${aMods}, vparamss: ${vparamss}")
    }
  }

  class SGLRUnitParser(unit: global.CompilationUnit) {
    def parse(): Tree = {
      val stratego_term = parser.parse(unit.source)
      val wrapped_term = Term(stratego_term)(unit.source)
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

  abstract class Term {
    var pos: Position = NoPosition

    def withPos(i: Int)(implicit src: SourceFile) = {
      pos = new OffsetPosition(src, i)
      this
    }
  }

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
    private def getOffset(term: IStrategoTerm) =
      term.getAttachment(ImploderAttachment.TYPE).getLeftToken.getStartOffset

    def apply(term: IStrategoTerm)(implicit src: SourceFile): Term = term match {
      case appl: StrategoAppl =>
        @@(appl.getName, appl.getAllSubterms() map {Term(_)}: _*) withPos(getOffset(term))
      case lst: StrategoList =>
        Lst(lst.getAllSubterms() map {Term(_)}: _*) withPos(getOffset(term))
      case str: StrategoString =>
        Str(str.stringValue()) withPos(getOffset(term))
      case unexp => throw new RuntimeException(s"Unhandled IStrategoTerm: ${unexp}")
    }
  }
}
