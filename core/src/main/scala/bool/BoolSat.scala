package bool

import nclogic.model.expr.Expr

trait BoolSat {

  def getSolution(e: Expr): Option[Expr] = {
    val it = iterator(e)
    if (it.hasNext) Some(it.next()) else None
  }

  def iterator(e: Expr): Iterator[Expr]

  def getAllSolutions(e: Expr): Set[Expr] = iterator(e).toSet
}
