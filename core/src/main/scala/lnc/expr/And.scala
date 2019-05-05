package lnc.expr

case class And(es: Set[Expr]) extends Expr {

  require(es.size > 1)

  override lazy val toString: String = {
    if (es.size == 1) es.head.toString
    else s"(${es.mkString(" & ")})"
  }
}


object And {
  def apply(es: Expr*): And = And(es.toSet)

  def fromSet[T <: Expr](es: Set[T]): And = And(es.map(_.asInstanceOf[Expr]))
}
