package nclogic.model.expr.ltl

import nclogic.model.expr.{Expr, N}

case class Finally(e: Expr) extends LTLOperator {
  override def toLNC(d: Int): Expr = {
    val se = e match {
      case op : LTLOperator => op.toLNC(d)
      case _ => e
    }

    Expr.or((for (i <- 0 to d) yield N(i, se)).toSet)
  }

  override def simplify: Expr = e match {
    case Finally(e1) => Finally(e1.simplify)
    case _ =>  Finally(e.simplify)
  }

  override def boolString: String = ???
}
