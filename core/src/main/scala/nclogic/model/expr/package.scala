package nclogic.model

package object expr {

  def N(e: Expr) = e.next

  def C(e: Expr) = e.change

  def G(e: Expr) = e.globally

  def F(e: Expr) = e.finallyExpr
}
