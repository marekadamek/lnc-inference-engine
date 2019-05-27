package lnc


package object expr {

  implicit def stringToVar(name: String): Expr = Var(name)

  implicit class ExprOps(e: Expr) {
    def &(e1: Expr): Expr = Expr.and(e, e1)

    def |(e1: Expr): Expr = Expr.or(e, e1)

    def unary_! : Expr = Expr.not(e)

    def ->(e1: Expr) = Impl(e, e1)

    def `<-`(e1: Expr) = Impl(e1, e)

    def <->(e1: Expr) = Eq(e, e1)
  }

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

  def C(level: Int, e: Expr): Expr = e match {
    case Change(Not(x), l) => C(l, x)
    case Change(x, l) =>
      C(l + level, x)
    case _ =>
      if (level == 0) e
      else Change(e, level)
  }


  def C(e: Expr): Expr = C(1, e)
}
