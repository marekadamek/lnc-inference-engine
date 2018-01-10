package nclogic.model.expr

case class Var(name: String) extends Expr {
  override def toString = name

  def isAtomic = true

  def simplify = this
}
