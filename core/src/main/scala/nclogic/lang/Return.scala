package nclogic.lang

import nclogic.model.expr.{Impl, Expr}

case class Return(e: Expr) extends Expr {
  override def simplify: Expr = Impl(Terminate, e).simplify

  override def isAtomic: Boolean = e.isAtomic
}
