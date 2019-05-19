package lnc.expr

/**
  * Logical disjunction of 2 or more formulas
  * @param es set of logical formulas connected with disjunction
  */
case class Or(es: Set[Expr]) extends Expr {
  require(es.size > 1)

  override lazy val toString: String = s"(${es.mkString(" | ")})"
}

object Or {
  def apply(es: Expr*): Or = new Or(es.toSet)
}