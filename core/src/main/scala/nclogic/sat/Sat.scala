package nclogic.sat

import nclogic._
import nclogic.model.expr._

object Sat {

  private def isContradictory(andList: Set[Set[Expr]]) = {
    if (andList.exists(_.isEmpty)) true
    else {
      val singles = andList.filter(_.size == 1).map(_.head)
      singles.contains(False) || singles.exists(e => singles.contains(Neg(e)))
    }
  }

  def solve(expr: Expr): Expr = expr :> makeSet :> solve :> toOr

  private def solve(cnf: Set[Set[Expr]]): Set[Set[Expr]] = {

    def solve(andList: Set[Set[Expr]], terms: Set[Expr]): Set[Set[Expr]] = andList match {
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


    def isFalse(and: Set[Expr]) = {
      and.exists(e1 => and.exists(e2 => e1 == Neg(e2).simplify))
    }

    solve(cnf, Set.empty)
      .map(and => and.map(_.simplify))
      .filterNot(isFalse)
  }

  private def makeSet(expr: Expr): Set[Set[Expr]] = expr match {
    case And(es) => es flatMap makeSet
    case Or(es) => Set((es flatMap makeSet).flatten)
    case x: Expr => Set(Set(x))
  }


  private def toOr(ands: Set[Set[Expr]]): Expr  = {
    if (ands.isEmpty) False
    else Or(ands map And).simplify
  }
}
