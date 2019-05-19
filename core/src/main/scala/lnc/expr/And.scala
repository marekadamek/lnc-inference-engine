package lnc.expr

/**
  * Logical conjunction of 2 or more formulas
  * @param es set of logical formulas connected with conjunction
  */
case class And(es: Set[Expr]) extends Expr {
  require(es.size > 1)

  override lazy val toString: String = s"(${es.mkString(" & ")})"
}


object And {
  def apply(es: Expr*): And = And(es.toSet)

  def fromSet[T <: Expr](es: Set[T]): And = And(es.map(_.asInstanceOf[Expr]))
}
