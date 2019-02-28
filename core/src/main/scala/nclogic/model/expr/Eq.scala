package nclogic.model.expr

case class Eq(e1: Expr, e2: Expr) extends Expr {
  override lazy val toString = s"($e1 <=> $e2)"

  lazy val simplify: Expr = {
    val (es1, es2) = (e1.simplify, e2.simplify)
    ((es1 & es2) | (!es1 & !es2)).simplify
  }

  override lazy val boolString: String = s"(${e1.boolString} = ${e2.boolString})"
}
