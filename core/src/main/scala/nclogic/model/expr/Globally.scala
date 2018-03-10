package nclogic.model.expr

case class Globally(e: Expr) extends TemporalExpr {
  override def toString = s"G($e)"

  def isAtomic = e.isAtomic

  /**
    * G(G(a)) <=> G(a)
    * G(a | b) <=> (a | b) & N(a | b) & N(N((a | b))) & ...
    * G(a & b) <=> G(a) & G(b)
    * G(a => b) <=> G(!a | b)
    * G(a <=> b) <=> G(!a | b) & G(a | !b)
    */
  def simplify(implicit level: Int): Expr = e match {
    case Globally(x) => Globally(x).simplify
    case _ if e.isAtomic => this
    case Or(_) => toNext(e, level).simplify
    case And(es) => And(es.map(_.globally)).simplify
    case Impl(_, _) => Globally(e.simplify).simplify
    case Eq(_, _) => Globally(e.simplify).simplify
    case _ => Globally(e.simplify).simplify
  }

  private def toNext(e: Expr, level: Int): Expr = And((0 to level).map(l => Next.createNext(e, l)).toSet)

  override def baseTerms: Set[Expr] = e.baseTerms

}
