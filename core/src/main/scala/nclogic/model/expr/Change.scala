package nclogic.model.expr

case class Change(e: Expr) extends TemporalExpr {
  override def toString = s"C($e)"

  def isAtomic = false

  def simplify(implicit level: Int): Expr = e match {
    case Neg(x) => Change(x).simplify
    case _ =>
      val es = e.simplify
      (es <-> N(!es)).simplify
  }

  override def baseTerms: Set[Expr] = e.baseTerms

  override val level: Int = e.level + 1
}
