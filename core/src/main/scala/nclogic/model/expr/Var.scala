package nclogic.model.expr

case class Var(name: String) extends Expr {

  override def toString: String = name.toString

  override def simplify(implicit level: Int): Expr = this

  override def isAtomic: Boolean = true

  override def baseTerms: Set[Expr] = Set(this)

  override def level: Int = 0
}