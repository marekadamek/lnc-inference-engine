package nclogic.model.expr

case class Cond(name: String, condition: () => Boolean) extends Expr {
  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  override def toString = name

  def check(): Boolean = condition()

  override def replaceVariables(s: SubstitutionSet): Expr = this
}
