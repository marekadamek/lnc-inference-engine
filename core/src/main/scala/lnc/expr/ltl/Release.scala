package lnc.expr.ltl

import lnc.expr.{Expr, N}

case class Release(e1: Expr, e2: Expr) extends LTLOperator {
  override def simplify: Expr = Release(e1.simplify, e2.simplify)
}
