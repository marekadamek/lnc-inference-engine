package nclogic.model.expr

case object False extends Expr {
  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  override def replaceVariables(s: SubstitutionSet): Expr = this

  override def baseTerms: Set[Expr] = Set.empty
}
