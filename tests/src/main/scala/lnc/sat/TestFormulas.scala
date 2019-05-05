package lnc.sat

import lnc.expr._

object TestFormulas {

  def positiveN(n: Int, d: Int): (Expr, String) = {
    val base = Expr.or((for (i <- 1 to n) yield Var(s"p$i").asInstanceOf[Expr]).toSet)
    val f = And((for (i <- 0 to d) yield N(i, base)).toSet)
    (f, s"N_${d}_$n")
  }

  def negativeN(n: Int, d: Int): (Expr, String) = {
    val base = Expr.or((for (i <- 1 to n) yield Var(s"p$i").asInstanceOf[Expr]).toSet)
    val f = And((for (i <- 0 until d) yield N(i, base)).toSet)
    val f1 = N(d, base)
    (f & !f1, s"not_N_${d}_$n")
  }

  def positiveC(n: Int, d: Int): (Expr, String) = {
    val base = Expr.or((for (i <- 1 to n) yield Var(s"p$i").asInstanceOf[Expr]).toSet)
    val f = C(d, base)
    (f, s"C_${d}_$n")
  }

  def negativeC(n: Int, d: Int): (Expr, String) = {
    val base = Expr.or((for (i <- 1 to n) yield Var(s"p$i").asInstanceOf[Expr]).toSet)
    (C(d, base) & N(d, base), s"not_C_${d}_$n")
  }


}