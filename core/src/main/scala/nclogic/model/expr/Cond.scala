package nclogic.model.expr

case class Cond(name: String, condition: () => Boolean) extends Expr {
  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  override def toString = name

  def check(): Boolean = condition()

  override def replaceVariables(s: SubstitutionSet): Expr = this

  override def baseTerms: Set[Expr] = Set(this)

  override def level: Int = 0
}
