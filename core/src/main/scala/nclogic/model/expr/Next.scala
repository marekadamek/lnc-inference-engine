package nclogic.model.expr

case class Next(e: Expr) extends TemporalExpr {
  override def toString = s"N($e)"
  override def toLatexString = e match {
    case Term(t) => t + "'"
    case Neg(Term(t)) => "\\lnot " + t + "'"
    case _ => "N($e)"
  }

  def isAtomic = e.isAtomic

  /**
   * N(a | b) <=> N(a) | N(b)
   * N(a & b) <=> N(a) & N(b)
   * N(a => b) <=> N(a) => N(b)
   * N(a <=> b) <=> N(a) <=> N(b)
   * N(!a) <=> !N(a)
   */
  def simplify: Expr = e match {
    case _ if e.isAtomic => this
    case Or(es) => Or(es.map(_.next)).simplify
    case And(es) => And(es.map(_.next)).simplify
    case Impl(p, q) => Impl(Next(p), Next(q)).simplify
    case Eq(p, q) => Eq(Next(p), Next(q)).simplify
    //case Neg(e1) => Neg(N(e1)).simplify
    case _ => Next(e.simplify).simplify
  }

  override def replaceVariables(s: SubstitutionSet): Expr = Next(e.replaceVariables(s))

  override def baseTerms: Set[Expr] = e.baseTerms
}
