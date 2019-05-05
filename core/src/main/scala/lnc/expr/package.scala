package lnc


package object expr {

  implicit def stringToVar(name: String): Expr = Var(name)

  def N(level: Int, e: Expr): Expr = e match {
    case True => True
    case False => False
    case Next(x, l) =>
      N(l + level, x)
    case _ =>
      if (level == 0) e
      else Next(e, level)
  }

  def N(e: Expr): Expr = N(1, e)

  def C(level: Int, e: Expr): Expr = Change(e, level)

  def C(e: Expr): Expr = C(1, e)
}
