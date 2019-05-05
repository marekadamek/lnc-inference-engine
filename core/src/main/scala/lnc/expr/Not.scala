package lnc.expr

case class Not(e: Expr) extends Expr {
  override lazy val toString = s"!$e"
}
