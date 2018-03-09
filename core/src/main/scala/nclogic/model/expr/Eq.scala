package nclogic.model.expr

case class Eq(e1: Expr, e2: Expr) extends Expr {
  override def toString = s"$e1 <=> $e2"
  override def toLatexString = s"${e1.toLatexString} \\Leftrightarrow ${e2.toLatexString}"

  def simplify = {
    val (es1, es2) = (e1.simplify, e2.simplify)
    ((!es1 | es2) & (es1 | !es2)).simplify
  }

  override def isAtomic = false

  override def replaceVariables(s: SubstitutionSet): Expr = Eq(e1.replaceVariables(s), e2.replaceVariables(s))

  override def baseTerms: Set[Expr] = e1.baseTerms ++ e2.baseTerms

  override def level: Int = Math.max(e1.level, e2.level)
}
