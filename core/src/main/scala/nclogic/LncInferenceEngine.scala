package nclogic

import nclogic.model.Types.{Neg, Expr}
import nclogic.solver.{DnfConverter, CnfConverter}

class LncInferenceEngine {

  def containsComplementaryPair(elems: Set[Expr]): Boolean = {
    def containsComplementaryPair(elems: List[Expr]): Boolean = elems match {
      case Nil => false
      case p::ps =>
        if (ps contains Neg(p).simplify) true
        else containsComplementaryPair(ps)
    }

    containsComplementaryPair(elems.toList)
  }

  def isTautology(formula: Expr) = CnfConverter.convert(formula) forall containsComplementaryPair

  def isContraTautology(formula: Expr) = DnfConverter.convert(formula) forall containsComplementaryPair

  def getPositiveValuations(formula: Expr) = None

  def getNegativeValuations(formula: Expr) = None

  def getHistoryGraph(formula: Expr) = formula :> DnfConverter.convert :> HistoryGraphFactory.create
}
