package bool

import nclogic.model.expr.Expr

trait BoolSat {

  def getSolution(e: Expr): Option[Set[Expr]]

  def getAllSolutions(e: Expr): Set[Set[Expr]]
}
