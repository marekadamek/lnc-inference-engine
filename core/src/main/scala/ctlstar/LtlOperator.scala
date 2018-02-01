package ctlstar

import nclogic.model.expr.Expr

trait LtlOperator

case class Until(e1: Expr, e2: Expr) extends LtlOperator

case class Release(e1: Expr, e2: Expr) extends LtlOperator

case class Globally(e: Expr) extends LtlOperator

case class GloballyFinally(e: Expr) extends LtlOperator

case class FinallyGlobally(e: Expr) extends LtlOperator

case class Finally(e: Expr) extends LtlOperator
