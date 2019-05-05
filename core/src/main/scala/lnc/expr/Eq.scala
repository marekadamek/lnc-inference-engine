package lnc.expr

case class Eq(e1: Expr, e2: Expr) extends Expr {
  override lazy val toString = s"($e1 <=> $e2)"
}
