package nclogic.model.expr

case object Terminate extends Expr {
  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  override def replaceVariables(s: SubstitutionSet): Expr = this
}
