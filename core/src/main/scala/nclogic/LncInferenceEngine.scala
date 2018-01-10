package nclogic

import nclogic.model._
import nclogic.model.converters.CnfConverter
import nclogic.model.expr.{False, Neg, Expr}
import nclogic.sat.Sat

object LncInferenceEngine {

  def isTautology(formula: Expr) = isContraTautology(Neg(formula))

  def isContraTautology(formula: Expr) = (formula :> CnfConverter.convert :> Sat.solve) == False

  def getHistoryGraph(formula: Expr) = HistoryGraph(formula)

}
