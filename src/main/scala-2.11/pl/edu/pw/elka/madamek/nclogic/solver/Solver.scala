package pl.edu.pw.elka.madamek.nclogic.solver

import pl.edu.pw.elka.madamek.nclogic.model.Types.{Expr, Neg}

object Solver {

  def isTautology(expr: Expr): Boolean = Cnf(Neg(expr)).resolute.isContradictory

}
