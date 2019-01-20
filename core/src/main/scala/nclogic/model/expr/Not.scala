package nclogic.model.expr

case class Neg(e: Expr) extends Expr {
  override def toString = "!" + e

  def isAtomic = e match {
    case Next(_) => false
    case Globally(_) => false
    case Finally(_) => false
    case Neg(_) => false
    case True => false
    case False => false
    case _ => e.isAtomic
  }

  def simplify(implicit level: Int): Expr =
    if (isAtomic) this
    else e match {
      case True => False
      case False => True
      case Neg(x) => x.simplify
      case And(es) => Or(es.map(_.not)).simplify
      case Or(es) => And(es.map(_.not)).simplify
      case Next(x) => Next(Neg(x)).simplify
      case Globally(x) => Finally(Neg(x)).simplify
      case Finally(x) => Globally(Neg(x)).simplify
      case _ => Neg(e.simplify).simplify
    }

  override def baseTerms: Set[Expr] = e.baseTerms

  override def level: Int = e.level
}
