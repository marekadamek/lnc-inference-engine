package nclogic.model

package object expr {

  def N(e: Expr) = e.next

  def C(e: Expr) = e.change

  def G(e: Expr) = e.always
}
