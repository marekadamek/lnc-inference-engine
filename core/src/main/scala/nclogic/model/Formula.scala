package nclogic.model

import nclogic.model.expr.{Var, Term, Expr}

class Formula(val e: Expr) {

  lazy val historyGraph = HistoryGraph(e)

  //def eval(assignments: Map[Var, Const]): Map[Var, Const] = ???
}

object Formula {
  implicit def exprToFormula(e: Expr) = new Formula(e)

  def future(e: Expr) = (path: List[Expr]) => path.exists(_.matches(e))

  def always(e: Expr) = (path: List[Expr]) => path.forall(_.matches(e))

  def until(e1: Expr, e2: Expr): List[Expr] => Boolean = {
    case Nil => true
    case e :: es =>
      val (e1Matches, e2Matches) = (e.matches(e1), e.matches(e2))
      if (e2Matches) true
      else if (e1Matches) until(e1, e2)(es) else false
  }

  def release(e1: Expr, e2: Expr) = ???
}