package nclogic.model.expr

case class Impl(e1: Expr, e2: Expr) extends Expr {
  override lazy val toString = s"($e1 -> $e2)"

  lazy val simplify: Expr = (!e1 | e2).simplify

  override def boolString: String = ???
}
