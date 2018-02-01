package nclogic.model.expr

case class Impl(e1: Expr, e2: Expr) extends Expr {
  override def toString = s"${e1.toString} => ${e2.toString}"

  def isAtomic = false

  def simplify = Expr.or(Neg(e1), e2)

  override def replaceVariables(s: SubstitutionSet): Expr = Impl(e1.replaceVariables(s), e2.replaceVariables(s))
}
