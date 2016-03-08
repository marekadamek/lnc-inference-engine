package pl.edu.pw.elka.madamek.nclogic.sat

import pl.edu.pw.elka.madamek.nclogic.model.Types.{Expr, Neg}
import pl.edu.pw.elka.madamek.nclogic.solver.Cnf

object Sat {

  def solve(cnf: Cnf): Set[Set[Expr]] = {
    def solve(andList: Set[Set[Expr]], terms: Set[Expr]): Set[Set[Expr]] = {
      if (andList.isEmpty) Set(terms)
      else {
        def notInContradiction = (t: Expr) => !terms.contains(Neg(t).simplify)
        andList.find(_.exists(notInContradiction)) match {
          case None => Set.empty
          case Some(clause) =>
            clause filter notInContradiction flatMap { t =>
              solve(andList.filterNot(_.contains(t)), terms + t)
            }
        }
      }
    }
    Set.empty
    //solve(cnf.andList, Set.empty)
  }
}
