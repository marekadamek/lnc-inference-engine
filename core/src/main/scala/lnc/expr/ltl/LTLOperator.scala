package lnc.expr.ltl

import lnc.expr._

trait LTLOperator extends Expr {
  def toLNC(d: Int) = LTLOperator.toLnc(this, d)
}

object LTLOperator {

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
      Expr.and((for (i <- 0 to d) yield N(i, se)).toSet)

    case Finally(x) =>
      val se = toLnc(x, d)
      Expr.or((for (i <- 0 to d) yield N(i, se)).toSet)

    case Until(e1, e2) =>
      val se1 = toLnc(e1, d)
      val se2 = toLnc(e2, d)

      val ors = for (i <- 0 to d) yield {
        val ands = (for (j <- 0 until i) yield N(j, se1)).toSet + N(i, se2)
        Expr.and(ands)
      }

      Expr.or(ors.toSet)


    case Release(e1, e2) =>
      val se1 = toLnc(e1, d)
      val se2 = toLnc(e2, d)

      val ands = for (i <- 0 to d) yield {
        val ors = (for (j <- 0 until i) yield N(j, se1)).toSet + N(i, se2)
        Expr.or(ors)
      }

      Expr.and(ands.toSet)

  }
}
