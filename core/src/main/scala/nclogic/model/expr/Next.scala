package nclogic.model.expr

case class Next(e: Expr) extends TemporalExpr {
  override def toString = s"N($e)"

  def isAtomic = e.isAtomic

  /**
    * N(a | b) <=> N(a) | N(b)
    * N(a & b) <=> N(a) & N(b)
    * N(a => b) <=> N(a) => N(b)
    * N(a <=> b) <=> N(a) <=> N(b)
    * !N(a) <=> N(!a) (implementation in Neg)
    */
  def simplify(implicit level: Int): Expr = e match {
    case _ if e.isAtomic => this
    case Or(es) => Or(es.map(_.next)).simplify
    case And(es) => And(es.map(_.next)).simplify
    case Impl(p, q) => Impl(Next(p), Next(q)).simplify
    case Eq(p, q) => Eq(Next(p), Next(q)).simplify
    case _ => Next(e.simplify).simplify
  }

  override def baseTerms: Set[Expr] = e.baseTerms
}

object Next {
  def createNext(term: Expr, level: Int): Expr = level match {
    case 0 => term
    case 1 => N(term)
    case _ => createNext(N(term), level - 1)
  }
}
