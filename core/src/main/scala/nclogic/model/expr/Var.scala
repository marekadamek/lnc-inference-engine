package nclogic.model.expr

case class Var(value: Any) extends Expr {

  override def toString: String = value.toString

  override def simplify(implicit level: Int): Expr = this

  override def isAtomic: Boolean = true

  override def baseTerms: Set[Expr] = Set(this)

  override def level: Int = 0
}