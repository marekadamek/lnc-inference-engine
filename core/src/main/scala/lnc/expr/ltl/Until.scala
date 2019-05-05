package lnc.expr.ltl

import lnc.expr.Expr

case class Until(e1: Expr, e2: Expr) extends LTLOperator {
  override def simplify: Expr = Until(e1.simplify, e2.simplify)
}
