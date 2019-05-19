package lnc.expr

/**
  * Logical equality of e1 <-> e2
  * @param e1 - first operand
  * @param e2 - second operand
  */
case class Eq(e1: Expr, e2: Expr) extends Expr {
  override lazy val toString = s"($e1 <-> $e2)"
}
