package nclogic.sat

import nclogic.java.model.expr._

object Sat {

  private def isContradictory(andList: Set[Set[Expr]]) = {
    if (andList.exists(_.isEmpty)) true
    else {
      val singles = andList.filter(_.size == 1).map(_.head)
      singles.contains(False) ||
        singles.exists(e1 => singles.exists(e2 => Expr.areContradictoryTerms(e1, e2)))
    }
  }

  def solve(expr: Expr): Expr = toExpr(solve(makeSet(expr)))

  def matches(e1: Expr, e2: Expr): Boolean = (e1, e2) match {
    case (t1@Var(_), t2@Var(_)) => t1 == t2
    case (t1@Var(_), g2@Globally(_)) => t1 == Expr.getNestedTerm(g2)

    case (n1@Neg(_), n2@Neg(_)) => n1 == n2
    case (n1@Neg(_), g2@Globally(_)) => n1 == Expr.getNestedTerm(g2)

    case (n1@Next(_), n2@Next(_)) => n1 == n2
    case (n1@Next(_), g2@Globally(_)) => Expr.getNestedTerm(n1) == Expr.getNestedTerm(g2)

    case (g1@Globally(_), t2@Var(_)) => Expr.getNestedTerm(g1) == t2
    case (g1@Globally(_), n2@Neg(_)) => Expr.getNestedTerm(g1) == n2
    case (g1@Globally(_), n2@Next(_)) => Expr.getNestedTerm(g1) == Expr.getNestedTerm(n2)
    case (g1@Globally(_), g2@Globally(_)) => Expr.getNestedTerm(g1) == Expr.getNestedTerm(g2)
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
            case Neg(e) => e
            case e => e
          }).get

          val neg = Neg(l).simplify
          //assign true
          val talResults = {
            val tal = andList
              .filterNot(and => contains(and, l))
              .map(and => and.filterNot(_ == neg))

            solve(tal, terms + l)
          }
          //assign neg
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

  private def toExpr(ands: Set[Set[Expr]]): Expr = {
    if (ands.isEmpty) False
    else Or.formSet(ands.map(a => And(a))).simplify
  }

}
