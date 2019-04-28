package nclogic.model.expr.ltl

import nclogic.model.expr.{Expr, N}

case class Always(e: Expr) extends LTLOperator {
  override def toLNC(d: Int): Expr = {
    val se = e match {
      case op : LTLOperator => op.toLNC(d)
      case _ => e
    }

    Expr.and((for (i <- 0 to d) yield N(i, se)).toSet)
  }

  override def simplify: Expr = e match {
    case Always(e1) => Always(e1.simplify)
    case _ =>  Always(e.simplify)
  }

  override def boolString: String = ???
}
