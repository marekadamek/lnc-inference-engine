package nclogic.model.expr

case class Impl(e1: Expr, e2: Expr) extends Expr {
  override def toString = s"(${e1.toString} => ${e2.toString})"
  override def toLatexString = s"(${e1.toLatexString} \\Rightarrow ${e2.toLatexString})"

  def isAtomic = false

  def simplify = Expr.or(Neg(e1), e2)

  override def replaceVariables(s: SubstitutionSet): Expr = Impl(e1.replaceVariables(s), e2.replaceVariables(s))

  override def baseTerms: Set[Expr] = e1.baseTerms ++ e2.baseTerms

  override def level: Int = Math.max(e1.level, e2.level)
}
