package lnc.expr

/**
  * Logical negation
  * @param e formula to be negated
  */
case class Not(e: Expr) extends Expr {
  override lazy val toString = s"!$e"
}
