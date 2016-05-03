package nclogic

import nclogic.model._
import nclogic.model.Types._
import nclogic.sat.Sat

object LncInferenceEngine {

  def isTautology(formula: Expr) = isContraTautology(Neg(formula))

  def isContraTautology(formula: Expr) = (formula :> CnfConverter.convert :> Sat.solve).isEmpty

  def getHistoryGraph(formula: Expr) = HistoryGraph(formula)

}
