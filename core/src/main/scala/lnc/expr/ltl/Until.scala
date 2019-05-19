package lnc.expr.ltl

import lnc.expr.Expr

/**
  * Represent temporal operator U in LTL
  * @param e1 first operand
  * @param e2 second operand
  */
case class Until(e1: Expr, e2: Expr) extends LTLOperator {
  override def simplify: Expr = Until(e1.simplify, e2.simplify)
}
