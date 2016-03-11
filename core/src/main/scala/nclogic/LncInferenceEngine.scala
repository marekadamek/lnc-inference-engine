package nclogic

import nclogic.model.{HistoryGraphFactory, DnfConverter, Types}
import nclogic.model.Types.{Expr, Neg}

object LncInferenceEngine {

  def containsComplementaryPair(elems: Set[Expr]): Boolean = {
    def containsComplementaryPair(elems: List[Expr]): Boolean = elems match {
      case Nil => false
      case p::ps =>
        if (ps contains Neg(p).simplify) true
        else containsComplementaryPair(ps)
    }

    containsComplementaryPair(elems.toList)
  }

  def isFalse(and: Set[Expr]) = and.contains(Types.Const(false)) || containsComplementaryPair(and)

  def isTautology(formula: Expr) = formula :> Neg :> isContraTautology

  def isContraTautology(formula: Expr) = DnfConverter.convert(formula.simplify) forall isFalse

  def getPositiveValuations(formula: Expr) = DnfConverter.convert(formula.simplify) filterNot isFalse

  def getNegativeValuations(formula: Expr) = formula :> Neg :> getPositiveValuations

  def getHistoryGraph(formula: Expr) = formula.simplify :> DnfConverter.convert :> HistoryGraphFactory.create
}
