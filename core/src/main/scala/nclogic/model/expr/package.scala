package nclogic.model

package object expr {

  def N(level: Int, e: Expr): Expr = if (level == 0) e else Next(e, level)

  def N(e: Expr): Expr = N(1, e)

  def C(level: Int, e: Expr): Expr = Change(e, level)

  def C(e: Expr): Expr = C(1, e)
}
