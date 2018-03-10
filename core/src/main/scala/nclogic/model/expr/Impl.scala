package nclogic.model.expr

case class Impl(e1: Expr, e2: Expr) extends Expr {
  override def toString = s"(${e1.toString} => ${e2.toString})"

  def isAtomic = false

  def simplify(implicit level: Int): Expr = (!e1 | e2).simplify

  override def baseTerms: Set[Expr] = e1.baseTerms ++ e2.baseTerms

  override def level: Int = Math.max(e1.level, e2.level)
}
