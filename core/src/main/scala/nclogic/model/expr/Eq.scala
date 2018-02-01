package nclogic.model.expr

case class Eq(e1: Expr, e2: Expr) extends Expr {
  override def toString = s"$e1 <=> $e2"

  def simplify = {
    val (es1, es2) = (e1.simplify, e2.simplify)
    Expr.or(Expr.and(es1, es2), Expr.and(Neg(es1), Neg(es2)))
  }

  override def isAtomic = false

  override def replaceVariables(s: SubstitutionSet): Expr = Eq(e1.replaceVariables(s), e2.replaceVariables(s))
}
