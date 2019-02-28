package nclogic.sat

import bool.{BoolSat, MiniSat}
import nclogic.model.{LNC, NormalFormConverter}
import nclogic.model.expr._

import scala.collection.mutable

trait BoolSATBasedLNCSat extends LNCSat {

  protected val boolSat: BoolSat

  def getAllBaseFormulaSolutions(e: Expr): Set[Expr] = {
    val normal = NormalFormConverter.convert(e)
    boolSat.getAllSolutions(normal)
  }
  def isSatisfiable(e: Expr): Boolean = {
    LNC.prefixFormula(e) match {
      case False => false
      case True => true
      case f => boolSat.iterator(f).hasNext
    }
  }

  def getSolution(e: Expr): Option[List[Expr]] = {
    val normal = NormalFormConverter.convert(e)
    val it = boolSat.iterator(normal)

    if (!it.hasNext) {
      None
    }
    else {
      val first = it.next()
      val satSolutions = mutable.Set.empty[Expr]
      satSolutions += first

      def getNext(suffix: Expr): Expr = {
        satSolutions.find(e => And(suffix, e).simplify != False) match {
          case Some(next) =>
            next
          case None =>
            satSolutions += it.next()
            getNext(suffix)
        }
      }

      def buildExample(solution: List[Expr]): List[Expr] = {
        val current = solution.head
        val next = getNext(LNC.suffix(current))
        if (solution.contains(next)) {
          solution.reverse.map(LNC.stripTemporalFromAnd)
        } else {
          buildExample(next :: solution)
        }
      }

      Some(buildExample(List(first)))
    }
  }

}
