package nclogic

import nclogic.model.DnfConverter.AndClause
import nclogic.model._
import nclogic.model.Types._
import nclogic.sat.Sat

//trait LncInferenceEngine {
//
//  def isTautology(e: Expr): Boolean
//
//  def isContraTautology(e: Expr): Boolean
//
//  def isTrue(formula: Expr, statement: Expr)
//}

object LncInferenceEngine {

  private def containsComplementaryPair(elems: Set[Expr]): Boolean = {
    def containsComplementaryPair(elems: List[Expr]): Boolean = elems match {
      case Nil => false
      case p :: ps =>
        if (ps contains Neg(p).simplify) true
        else containsComplementaryPair(ps)
    }

    containsComplementaryPair(elems.toList)
  }

  private def isFalse(and: AndClause) = and.contains(Types.Const(false)) || containsComplementaryPair(and)

  def isTautology(formula: Expr) = getNegativeValuations(formula).isEmpty

  @Deprecated
  def isContraTautology(formula: Expr) = DnfConverter.convert(formula.simplify) forall isFalse

  def getPositiveValuations(formula: Expr) = filterPositiveValuations(formula, generateValuations(formula))

  def getNegativeValuations(formula: Expr) = {
    val valuations = generateValuations(formula)
    valuations -- filterPositiveValuations(formula, valuations)
  }

  def getHistoryGraph(formula: Expr) = HistoryGraph(formula)

  private def filterPositiveValuations(formula: Expr, valuations: Set[Set[Expr]]): Set[Set[Expr]] = {
    val nodes = HistoryGraph(formula).nodes
    valuations filter { vars =>
      nodes exists {
        _ forall vars.contains
      }
    }
  }

  private def extractVars(expr: Expr): Set[Var] = expr match {
    case Var(x) => Set(Var(x))
    case Neg(e) => extractVars(e)
    case N(e) => extractVars(e)
    case C(e) => extractVars(e)
    case And(e1, e2) => extractVars(e1) ++ extractVars(e2)
    case Or(e1, e2) => extractVars(e1) ++ extractVars(e2)
    case _ => extractVars(expr.simplify)
  }

  private def generateValuations(vars: Set[Var]): Set[Set[Expr]] = {
    if (vars.isEmpty) Set.empty
    else {
      val tails = generateValuations(vars.tail)
      val v = vars.head
      if (tails.isEmpty) Set(Set(v), Set(Neg(v)))
      else tails.map(_ + v) ++ tails.map(_ + Neg(v))
    }
  }

  private def generateValuations(e: Expr): Set[Set[Expr]] = e :> extractVars :> generateValuations
}
