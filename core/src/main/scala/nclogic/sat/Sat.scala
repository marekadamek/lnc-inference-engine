package nclogic.sat

import nclogic.model.CnfConverter.CNF
import nclogic.model.DnfConverter.DNF
import nclogic.model.Types.{Neg, Expr}

object Sat {
  def solve(cnf: CNF): DNF = {
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
    solve(cnf, Set.empty)
  }
}
