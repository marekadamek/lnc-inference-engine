package nclogic

import pl.edu.pw.elka.madamek.nclogic.model.Types.Expr


class LncInferenceEngine {

  def isTautology(formula: Expr) = false

  def isContraTautology(formula: Expr) = false

  def getPositiveValuations(formula: Expr) = None

  def getNegativeValuations(formula: Expr) = None

  def getHistoryGraph(formula: Expr) = None
}
