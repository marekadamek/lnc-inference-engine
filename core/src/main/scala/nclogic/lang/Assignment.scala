package nclogic.lang

import nclogic.model.expr.{Var, Expr}

case class Assignment(v: Var, e: Expr) extends Expr{
  override def simplify: Expr = Assignment(v, e.simplify)

  override def isAtomic: Boolean = e.isAtomic
}
