package nclogic.model.expr

case class Finally(e: Expr) extends TemporalExpr {
  override def toString = s"F($e)"

  def isAtomic = false

  /**
    * F(F(a)) <=> F(a)
    * F(a | b) <=> F(a) | F(b)
    * F(a & b) <=> (a & b) | N(a & b) | N(N((a & b))) | ...
    * F(a => b) <=> F(!a) | F(b)
    * F(a <=> b) <=> F((!a | b) & (a | !b))
    */
  def simplify(implicit level: Int): Expr = e match {
    case Finally(x) => Finally(x).simplify
    case _ if e.isAtomic => toNext(e, level)
    case Or(es) => And(es.map(_.finallyExpr)).simplify
    case And(_) => toNext(e, level).simplify
    case Impl(_, _) => Finally(e.simplify).simplify
    case Eq(_, _) => Finally(e.simplify).simplify
    case _ => Finally(e.simplify).simplify
  }

  private def toNext(e: Expr, level: Int): Expr = Or((0 to level).map(l => Next.createNext(e, l)).toSet)

  override def baseTerms: Set[Expr] = e.baseTerms

  override val level: Int = 0

}
