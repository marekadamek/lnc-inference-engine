package nclogic.model.expr

case class Change(e: Expr) extends TemporalExpr {
  override def toString = s"C($e)"

  def isAtomic = false

  def simplify: Expr = e match {
    case Neg(x) => Change(x).simplify
    case _ =>
      val es = e.simplify
      Eq(es, Next(Neg(es))).simplify
  }

  override def replaceVariables(s: SubstitutionSet): Expr = Change(e.replaceVariables(s))

  override def baseTerms: Set[Expr] = e.baseTerms
}
