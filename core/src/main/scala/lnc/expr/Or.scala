package lnc.expr

case class Or(es: Set[Expr]) extends Expr {

  require(es.size > 1)

  override lazy val toString: String = {
    if (es.size == 1) es.head.toString
    else s"(${es.mkString(" | ")})"
  }
}

object Or {
  def apply(es: Expr*): Or = new Or(es.toSet)
}