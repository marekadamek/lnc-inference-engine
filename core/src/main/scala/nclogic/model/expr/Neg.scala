package nclogic.model.expr

case class Neg(e: Expr) extends Expr {
  override def toString = "!" + e

  def isAtomic = e match {
    case Next(_) => false
    case Neg(_) => false
    case _ => e.isAtomic
  }

  def simplify: Expr =
    if (isAtomic) this
    else e match {
      case True => False
      case False => True
      case Var(_) => this
      case Neg(x) => x.simplify
      case And(es) => Or(es.map(_.not)).simplify
      case Or(es) => And(es.map(_.not)).simplify
      case Next(x) => Next(Neg(x)).simplify
      case _ => Neg(e.simplify).simplify
    }
}
