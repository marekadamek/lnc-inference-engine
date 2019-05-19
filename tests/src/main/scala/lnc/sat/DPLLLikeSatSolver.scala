package lnc.sat

import lnc.bool.BoolSatIterator
import lnc.expr._


case class DPLLLikeSatSolver(expr: Expr) extends BoolSatIterator {
  private var toDo: List[(List[Expr], Expr)] = expr match {
    case False => Nil
    case True => List((Nil, True))
    case _ =>
      val startingTerm = getTerm(List(expr)).get
      List((List(startingTerm), expr), (List(!startingTerm), expr))
  }


  private def getTerm(toDo: List[Expr]): Option[Expr] = toDo match {
    case Nil => None
    case head :: tail => head match {
      case True | False => getTerm(tail)
      case _ if Expr.isTerm(head) => Some(head)
      case And(es) => getTerm(es.toList ::: tail)
      case Or(es) => getTerm(es.toList ::: tail)
      case Impl(e1, e2) => getTerm(e1 :: e2 :: tail)
      case Eq(e1, e2) => getTerm(e1 :: e2 :: tail)
      case Not(e) => getTerm(e :: tail)
    }
  }

  private def isContradictory(es: List[Expr]): Boolean = {
    var posMap = Set.empty[Expr]
    var negMap = Set.empty[Expr]

    es.foreach {
      case False | Next(False, _) =>
        return true

      case e@(Var(_) | Next(Var(_), _)) =>
        if (negMap.contains(e)) {
          return true
        }
        posMap = posMap + e

      case Not(Var(v)) =>
        if (posMap.contains(Var(v))) {
          return true
        }

        negMap = negMap + Var(v)

      case Not(Next(Var(v), l)) =>
        if (posMap.contains(Next(Var(v), l))) {
          return true
        }

        negMap = negMap + Next(Var(v), l)

      case _ =>
    }

    false
  }

  private def expand(node: (List[Expr], Expr)): Either[List[(List[Expr], Expr)], Set[Expr]] = {
    val (terms, e) = node

    val applied = Expr.setTrue(e, Set(terms.head))
    if (applied match {
      case False | Not(True) | Next(False, _) | Not(Next(True, _)) => true
      case _ => false
    }) {
      return Left(Nil)
    }

    getTerm(List(applied)) match {
      case None =>
        Right(terms.toSet)
      case Some(t) =>

        var newNodes = List.empty[(List[Expr], Expr)]

        if (!isContradictory(!t :: terms)) {
          newNodes = (!t :: terms, applied) :: newNodes
        }

        if (!isContradictory(t :: terms)) {
          newNodes = (t :: terms, applied) :: newNodes
        }

        Left(newNodes)

    }
  }

  def next(): Option[Set[Expr]] = {
    var result = Option.empty[Set[Expr]]

    while (toDo.nonEmpty && result.isEmpty) {
      val node = toDo.head
      toDo = toDo.tail

      node._2 match {
        case True =>
          result = Some(Set.empty)
          toDo = Nil
        case False =>
          toDo = Nil
        case _ =>
          expand(node) match {
            case Right(solution) =>
              result = Some(solution)
            case Left(next) =>
              toDo = next ::: toDo
          }
      }
    }
    result
  }
}


