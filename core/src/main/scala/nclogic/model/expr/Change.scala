package nclogic.model.expr

case class Change(e: Expr) extends Expr {
  override def toString = s"C($e)"

  def isAtomic = false

  def simplify = e match {
    case Neg(x) => Change(x).simplify
    case _ =>
      val es = e.simplify
      Eq(es, Neg(Next(es))).simplify
  }

}
