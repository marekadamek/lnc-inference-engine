package nclogic.sat

import nclogic.model.expr._

object DPLL {

  private def isContradictory(andList: Set[Set[Expr]]) = {
    if (andList.exists(_.isEmpty)) true
    else {
      val singles = andList.filter(_.size == 1).map(_.head)
      singles.contains(False) || Expr.isContradictory(singles)
    }
  }

  def solve(expr: Expr): Set[Set[Expr]] = makeSet(expr)

  def matches(e1: Expr, e2: Expr): Boolean = (e1, e2) match {
    case (t1@Var(_), t2@Var(_)) => t1 == t2
    case (n1@Not(_), n2@Not(_)) => n1 == n2
    case (n1@Next(_, _), n2@Next(_, _)) => n1 == n2
    case _ => false
  }

  private def contains(terms: Set[Expr], e: Expr): Boolean = terms.exists(t => matches(t, e))

  private def solve(cnf: Set[Set[Expr]]): Set[Set[Expr]] = {

    def solve(andList: Set[Set[Expr]], terms: Set[Expr]): Set[Set[Expr]] = {
      andList.toList match {
        case Nil =>
          Set(terms)
        case s if isContradictory(s.toSet) =>
          Set.empty
        case _ =>
          val l = andList.head.headOption.map({
            case Not(e) => e
            case e => e
          }).get

          val neg = Not(l).simplify
          //assign true
          val talResults = {
            val tal = andList
              .filterNot(and => contains(and, l))
              .map(and => and.filterNot(_ == neg))

            solve(tal, terms + l)
          }
          //assign false
          val nalResults = {
            val nal = andList
              .filterNot(and => contains(and, neg))
              .map(and => and.filterNot(_ == l))

            solve(nal, terms + neg)
          }

          talResults ++ nalResults
      }
    }

    solve(cnf, Set.empty)
      .filterNot(_ == False)
  }

  private def makeSet(expr: Expr): Set[Set[Expr]] = expr match {
    case And(es) => es flatMap makeSet
    case Or(es) => Set((es flatMap makeSet).flatten)
    case x: Expr => Set(Set(x))
  }

}
