package nclogic.sat

import nclogic.model.LNC
import nclogic.model.expr._


case class TableAuxBDD(expr: Expr) {
  private var toDo = List((Set.empty[Expr], expr))
  private var visited = Set.empty[(Set[Expr], Expr)]


  def getTerm(toDo: List[Expr]): Option[Expr] = toDo match {
    case Nil => None
    case head :: tail => head match {
      case True | False => getTerm(tail)
      case _ if head.isTerm => Some(head)
      case And(es) => getTerm(es.toList ::: tail)
      case Or(es) => getTerm(es.toList ::: tail)
      case Impl(e1, e2) => getTerm(e1 :: e2 :: tail)
      case Eq(e1, e2) => getTerm(e1 :: e2 :: tail)
      case Not(e) => getTerm(e :: tail)
    }
  }

  private def isContradictory(es: Set[Expr]): Boolean = {
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

  private def expand(node: (Set[Expr], Expr)): Either[List[(Set[Expr], Expr)], Set[Expr]] = {
    val (terms, e) = node
    if (e match {
      case False | Not(True) | Next(False, _) | Not(Next(True, _)) => true
      case _ => false
    }) {
      return Left(Nil)
    }

    getTerm(List(e)) match {
      case None => Right(terms)
      case Some(t) =>
        var newNodes = List.empty[(Set[Expr], Expr)]

        if (!isContradictory(terms + !t)) {
          val neg = TableAux.setTrue(e, Set(!t))
          newNodes = (terms + !t, neg) :: newNodes
        }

        if (!isContradictory(terms + t)) {
          val pos = TableAux.setTrue(e, Set(t))
          newNodes = (terms + t, pos) :: newNodes
        }

        Left(newNodes)

    }
  }

  def next(): Option[Set[Expr]] = {
    var result = Option.empty[Set[Expr]]

    while (toDo.nonEmpty && result.isEmpty) {
      val node = toDo.head
      toDo = toDo.tail
      if (!visited.contains(node)) {
        visited = visited + node

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

object TableAuxBDD {

  def isSatisfiable(expr: Expr): Boolean = solveOne(expr).isDefined

  def solveOne(expr: Expr): Option[Set[Expr]] = TableAux(expr).next()

  def solveAll(expr: Expr): Set[Set[Expr]] = {
    val tableAux = TableAuxBDD(expr)
    var result = List.empty[Set[Expr]]
    var elem = tableAux.next()

    while (elem.isDefined) {
      result = elem.get :: result
      elem = tableAux.next()

    }
    result.toSet
  }
}


