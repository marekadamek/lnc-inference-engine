package nclogic.model.expr

case object Terminate extends Expr {
  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  override def replaceVariables(s: SubstitutionSet): Expr = this

  override def baseTerms: Set[Expr] = Set(this)

  override def level: Int = 0
}
