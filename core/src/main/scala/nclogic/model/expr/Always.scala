package nclogic.model.expr

case class Always(e: Expr) extends TemporalExpr {
  override def toString = s"G($e)"

  def isAtomic = e.isAtomic

  /**
   * G(a | b) <=> G(a) | G(b)
   * G(a & b) <=> G(a) & G(b)
   * G(a => b) <=> G(a) => G(b)
   * G(a <=> b) <=> G(a) <=> G(b)
   */
  def simplify: Expr = e match {
    case _ if e.isAtomic => this
    case Or(es) => Or(es.map(_.always)).simplify
    case And(es) => And(es.map(_.always)).simplify
    case Impl(p, q) => p.always -> q.always
    case Eq(p, q) => p.always <-> q.always
    //case Neg(e1) => Neg(N(e1)).simplify
    case _ => e.simplify.always
  }

}
