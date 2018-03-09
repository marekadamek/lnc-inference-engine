package nclogic.sat

import nclogic._
import nclogic.model.expr._

object Sat {

  private def isContradictory(andList: List[List[Expr]]) = {
    if (andList.exists(_.isEmpty)) true
    else {
      val singles = andList.filter(_.lengthCompare(1) == 0).map(_.head)
      singles.contains(False) || singles.exists(e => singles.contains(Neg(e)))
    }
  }

  def solve(expr: Expr): Expr = expr :> makeSet :> solve :> toOr

  private def solve(cnf: List[List[Expr]]): List[List[Expr]] = {

    def solve(andList: List[List[Expr]], terms: List[Expr]): List[List[Expr]] = {
      andList match {
        case Nil => List(terms)
        case s if isContradictory(s) => Nil
        case _ =>
          val l = andList.head.headOption.map({
            case Neg(e) => e
            case e => e
          }).get

          val neg = Neg(l).simplify
          //assign true
          val tal = andList.filterNot(_.contains(l)).map(and => and.filterNot(_ == neg))
          //assign neg
          val nal = andList.filterNot(_.contains(neg)).map(and => and.filterNot(_ == l))
          solve(tal, terms ++ List(l)) ++ solve(nal, terms ++ List(neg))
      }
    }


    def isFalse(and: List[Expr]) = {
      and.exists(e1 => and.exists(e2 => e1 == Neg(e2).simplify))
    }

    solve(cnf, List.empty)
      .filterNot(isFalse)
  }

  private def makeSet(expr: Expr): List[List[Expr]] = expr match {
    case And(es) => es flatMap makeSet
    case Or(es) => List((es flatMap makeSet).flatten)
    case x: Expr => List(List(x))
  }

  private def toOr(ands: List[List[Expr]]): Expr = {
    if (ands.isEmpty) False
    else Or(ands map And).simplify
  }

}
