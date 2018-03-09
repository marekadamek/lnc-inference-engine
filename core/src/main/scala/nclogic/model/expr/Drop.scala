package nclogic.model.expr

case class Drop(e: Expr) extends TemporalExpr {
  override def toString = s"D($e)"

  def isAtomic = e.isAtomic

  def simplify: Expr = e match {
    case Cond(_,_)  => this
    case Block(_,_) => this
    case And(es) => And(es.map(Drop)).simplify
    case _ => throw new IllegalArgumentException("Unsupported expr for drop: " + e)

  }

  override def replaceVariables(s: SubstitutionSet): Expr = Next(e.replaceVariables(s))

  override def baseTerms: Set[Expr] = Set(this)
}