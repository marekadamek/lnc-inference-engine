package lnc.expr.ltl

import lnc.expr.Expr

/**
  * Represents temporal operator F in LTL
  * @param e operand
  */
case class Finally(e: Expr) extends LTLOperator {
  override def simplify: Expr = e match {
    case Finally(e1) => Finally(e1.simplify)
    case _ => Finally(e.simplify)
  }
}
