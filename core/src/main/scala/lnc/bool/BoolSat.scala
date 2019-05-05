package lnc.bool

import lnc.expr.Expr

trait BoolSatIterator {
  def next(): Option[Set[Expr]]
}

trait BoolSat {

  def getSolution(e: Expr): Option[Set[Expr]] = iterator(e).next()

  def getAllSolutions(e: Expr): Set[Set[Expr]] = {
    val it = iterator(e)
    var result = Set.empty[Set[Expr]]
    var elem = it.next()

    while (elem.isDefined) {
      result = result + elem.get
      elem = it.next()

    }

    result
  }

  def iterator(e: Expr): BoolSatIterator

  def isSatisfiable(expr: Expr): Boolean = getSolution(expr).isDefined
}
