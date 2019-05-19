package lnc.expr.ltl

import lnc.expr._
import lnc.expr.Expr

/**
  * Base trait for LTL temporal operators that do not exist in LNC
  */
trait LTLOperator extends Expr {

  /**
    * Converts LTL formula to equivalent LNC formula of given depth
    * @param d depth of output formula
    * @return LNC formula equivalent to input formula
    */
  def toLNC(d: Int): Expr = LTLOperator.toLnc(this, d)
}

object LTLOperator {
  import Expr._

  /**
    * Converts LTL formula to equivalent LNC formula of given depth
    * @param d depth of output formula
    * @return LNC formula equivalent to input formula
    */
  def toLnc(e: Expr, d: Int): Expr = e match {
    case True | False | Var(_) => e
    case And(es) => And(es.map(toLnc(_, d)))
    case Or(es) => Or(es.map(toLnc(_, d)))
    case Impl(e1, e2) => Impl(toLnc(e1, d), toLnc(e2, d))
    case Eq(e1, e2) => Eq(toLnc(e1, d), toLnc(e2, d))
    case Not(x) => Not(toLnc(x, d))
    case Next(x, l) => Next(toLnc(x, d), l)
    case Change(x, l) => Change(toLnc(x, d), l)

    case Always(x) =>
      val se = toLnc(x, d)
      and((for (i <- 0 to d) yield N(i, se)).toSet)

    case Finally(x) =>
      val se = toLnc(x, d)
      or((for (i <- 0 to d) yield N(i, se)).toSet)

    case Until(e1, e2) =>
      val se1 = toLnc(e1, d)
      val se2 = toLnc(e2, d)

      val ors = for (i <- 0 to d) yield {
        val ands = (for (j <- 0 until i) yield N(j, se1)).toSet + N(i, se2)
        and(ands)
      }

      or(ors.toSet)

    case Release(e1, e2) =>
      val se1 = toLnc(e1, d)
      val se2 = toLnc(e2, d)

      val ands = for (i <- 0 to d) yield {
        val ors = (for (j <- 0 until i) yield N(j, se1)).toSet + N(i, se2)
        or(ors)
      }

      and(ands.toSet)
  }
}
