package nclogic.model.expr

case class Eq(e1: Expr, e2: Expr) extends Expr {
  override def toString = s"$e1 <=> $e2"

  def simplify(implicit level: Int) = {
    val (es1, es2) = (e1.simplify, e2.simplify)
    val nes1 = Neg(es1).simplify
    val nes2 = Neg(es2).simplify
    ((nes1 | es2) & (es1 | nes2)).simplify
  }

  override def isAtomic = false

  override def baseTerms: Set[Expr] = e1.baseTerms ++ e2.baseTerms

  override def level: Int = Math.max(e1.level, e2.level)
}
