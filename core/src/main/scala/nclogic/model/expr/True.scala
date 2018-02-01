package nclogic.model.expr

case object True extends Expr {
  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  override def replaceVariables(s: SubstitutionSet): Expr = this
}
