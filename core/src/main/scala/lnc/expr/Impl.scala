package lnc.expr

/**
  * Logical implication e1 -> e2
  * @param e1 - antecedent
  * @param e2 - consequent
  */
case class Impl(e1: Expr, e2: Expr) extends Expr {
  override lazy val toString = s"($e1 -> $e2)"
}
