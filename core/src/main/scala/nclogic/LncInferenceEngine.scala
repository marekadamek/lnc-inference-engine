package nclogic

import nclogic.model._
import nclogic.model.converters.CnfConverter
import nclogic.model.expr._
import nclogic.sat.Sat

object LncInferenceEngine {

  def isTautology(formula: Expr): Boolean = isContraTautology(Neg(formula))

  def isContraTautology(formula: Expr): Boolean = Sat.solve(CnfConverter.convert(formula)) == False

  def getHistoryGraph(formula: Expr) = HistoryGraph(formula)

  def getNext(clause: Set[Expr]): Set[Expr] = clause
    .filter(_.isInstanceOf[Next])
    .map(_.asInstanceOf[Next])
    .map(_.e)

}
