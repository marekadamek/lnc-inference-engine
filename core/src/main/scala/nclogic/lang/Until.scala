package nclogic.lang

import nclogic.model.expr.Expr

class Until(block: => Boolean) extends Expr {
  override def simplify: Expr = this

  override def isAtomic: Boolean = true

  def value() = block
}
