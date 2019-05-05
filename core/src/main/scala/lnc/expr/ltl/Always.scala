package lnc.expr.ltl

import lnc.expr.{Expr, N}

case class Always(e: Expr) extends LTLOperator {
  override def simplify: Expr = e match {
    case Always(e1) => Always(e1.simplify)
    case _ =>  Always(e.simplify)
  }
}
