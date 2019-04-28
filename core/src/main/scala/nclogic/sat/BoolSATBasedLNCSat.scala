package nclogic.sat

import bool.{BoolSat, MiniSat}
import nclogic.model.{LNC, NormalFormConverter}
import nclogic.model.expr._

import scala.collection.mutable

trait BoolSATBasedLNCSat extends LNCSat {

  val boolSat: BoolSat

//  def getAllBaseFormulaSolutions(e: Expr): Set[Expr] = {
//    val normal = NormalFormConverter.convertToNormalForm(e)
//    boolSat.getAllSolutions(normal)
//  }
//  def isSatisfiable(e: Expr): Boolean = {
//    LNC.prefixFormula(e) match {
//      case False => false
//      case True => true
//      case f => boolSat.getNext(f).isDefined
//    }
//  }
//
//  def getSolution(e: Expr): Option[List[Expr]] = {
//    val normal = NormalFormConverter.convert(e)
//
//    val first = boolSat.getNext(normal)
//
//    if (first.isEmpty) {
//      None
//    }
//    else {
//      val satSolutions = mutable.Set.empty[Expr]
//      satSolutions += first.get
//
//      def getNext(suffix: Expr): Expr = {
//        satSolutions.find(e => And(suffix, e).simplify != False) match {
//          case Some(next) =>
//            next
//          case None =>
//            satSolutions += boolSat.getNext(normal).get
//            getNext(suffix)
//        }
//      }
//
//      def buildExample(solution: List[Expr]): List[Expr] = {
//        val current = solution.head
//        val next = getNext(LNC.suffix(current))
//        if (solution.contains(next)) {
//          solution.reverse.map(LNC.stripTemporalFromAnd)
//        } else {
//          buildExample(next :: solution)
//        }
//      }
//
//      Some(buildExample(List(first.get)))
//    }
//  }

}
