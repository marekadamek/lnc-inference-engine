package nclogic.model.expr.ltl

import nclogic.model.expr.{Expr, N}

case class Release(e1: Expr, e2: Expr) extends LTLOperator {
  override def toLNC(d: Int): Expr = {
    val se1 = e1 match {
      case op : LTLOperator => op.toLNC(d)
      case _ => e1
    }

    val se2 = e2 match {
      case op : LTLOperator => op.toLNC(d)
      case _ => e2
    }

    val ands = for (i <- 0 to d) yield {
      val ors = (for (j <- 0 until i) yield N(j, se1)).toSet + N(i, se2)
      Expr.or(ors)
    }

    Expr.and(ands.toSet)
  }

  override def simplify: Expr = Release(e1.simplify, e2.simplify)

  override def boolString: String = ???
}
