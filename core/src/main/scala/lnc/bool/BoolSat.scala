package lnc.bool

import lnc.expr.Expr

/**
  * Base trait for boolean formula solutions iterator
  */
trait BoolSatIterator {
  def next(): Option[Set[Expr]]
}

/**
  * Base trait for boolean SAT solver
  */
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
}
