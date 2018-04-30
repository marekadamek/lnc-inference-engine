package nclogic.model.expr

case object True extends Expr {
  override def simplify(implicit level: Int): Expr = this

  override def isAtomic: Boolean = true

  override def baseTerms: Set[Expr] = Set.empty

  override def level: Int = 0
}
