package lnc.expr.ltl

import lnc.expr.Expr

/**
  * Represent temporal operator G in LTL
  * @param e operand
  */
case class Always(e: Expr) extends LTLOperator {
  override lazy val simplify: Expr = e match {
    case Always(e1) => Always(e1.simplify)
    case _ => Always(e.simplify)
  }
}
