package nclogic.sat

import nclogic.model.expr.Expr

trait LNCSat {

  def isSatisfiable(e: Expr): Boolean

  def getSolution(e: Expr): Option[List[Expr]]

  //def getAllSolutions(e: Expr): Seq[Set[Expr]]

}
