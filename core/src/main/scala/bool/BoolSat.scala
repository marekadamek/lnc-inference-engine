package bool

import nclogic.model.expr.Expr
import nclogic.sat.TableAuxBDD

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


  def solveAll(expr: Expr): Set[Set[Expr]] = {
    val tableAux = TableAuxBDD(expr)
    var result = List.empty[Set[Expr]]
    var elem = tableAux.next()

    while (elem.isDefined) {
      result = elem.get :: result
      elem = tableAux.next()

    }
    result.toSet
  }
}
