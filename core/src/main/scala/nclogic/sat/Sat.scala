package nclogic.sat

import nclogic.model.CnfConverter.CNF
import nclogic.model.DnfConverter.{AndClause, DNF}
import nclogic.model.Types.{Neg, Expr}

object Sat {

  def isContradictory(andList: Set[Set[Expr]]) = {
    if (andList.exists(_.isEmpty)) true
    else {
      val singles = andList.filter(_.size == 1).map(_.head)
      singles.exists(e => singles.contains(Neg(e)))
    }
  }

  def solve(cnf: CNF): DNF = {

    def solve(andList: Set[Set[Expr]], terms: Set[Expr]): Set[AndClause] = andList match {
      case s if s.isEmpty => Set(terms)
      case s if isContradictory(s) => Set.empty
      case _ =>
        val l = andList.head.headOption map {
          case Neg(e) => e
          case e => e
        } get

        val neg = Neg(l)
        //assign true
        val tal = andList.filterNot(_.contains(l)).map(and => and - neg)
        //assign neg
        val nal = andList.filterNot(_.contains(neg)).map(and => and - l)
        solve(tal, terms + l) ++ solve(nal, terms + neg)
    }


    solve(cnf, Set.empty)
  }

}
