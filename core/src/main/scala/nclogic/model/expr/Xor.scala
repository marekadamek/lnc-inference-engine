package nclogic.model.expr

case class Xor(e1: Expr, e2: Expr) extends Expr {
  override def toString = s"${e1.toString} ^ ${e2.toString}"

  def isAtomic = false

  def simplify = (e1 & !e2) | (!e1 & e2)

  override def replaceVariables(s: SubstitutionSet): Expr = Impl(e1.replaceVariables(s), e2.replaceVariables(s))

  override def baseTerms: Set[Expr] = e1.baseTerms ++ e2.baseTerms

  override def level: Int = Math.max(e1.level, e2.level)
}
