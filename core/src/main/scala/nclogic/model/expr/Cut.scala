package nclogic.model.expr

case object Cut extends Expr {

  def isAtomic = true

  def simplify: Expr = this

  override def replaceVariables(s: SubstitutionSet): Expr = this
}
