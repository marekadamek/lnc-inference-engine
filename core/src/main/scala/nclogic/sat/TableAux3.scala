package nclogic.sat

import nclogic.model.expr._
import nclogic.model.{LNC, NormalFormConverter}

import scala.collection.mutable

case class Node2(terms: Set[Expr], expr: Expr)

case class TableAux3(expr: Expr) {
  private var toDo = List(Node2(Set.empty, expr))
  private val visited = mutable.Set.empty[Expr]

  def getNext: Option[Expr] = {
    var result = Option.empty[Expr]

    while (toDo.nonEmpty && result.isEmpty) {
      val node = toDo.head
      toDo = toDo.tail
      if (!visited.contains(node.expr)) {
        visited += node.expr

        node.expr match {
          case False =>
          case True =>
            result = LNC.basicSimplify(And(node.terms)) match {
              case False => None
              case e => Some(e)
            }
          case _ =>
            val terms = TableAux3.getSatisfiableTerms(node.expr)
            toDo = Node2(node.terms ++ terms, TableAux.setTrue(node.expr, terms)) :: toDo
        }
      }
    }

    result
  }
}

object TableAux3 {

  def getTerm(expr: Expr): Expr = {
    def loop(toDo: List[Expr]): Expr = toDo match {
      case Nil => throw new Exception("imposible")
      case e :: tail =>
        e match {
          case True | False | Next(True, _) | Next(False, _) =>
            loop(tail)

          case _ if e.isTerm => e match {
            case Not(x) => x
            case _ => e
          }

          case And(es) =>
            loop(es.toList ++ tail)

          case Or(es) =>
            loop(es.toList ++ tail)

          case Eq(e1, e2) =>
            loop(e1 :: e2 :: tail)

          case Impl(e1, e2) =>
            loop(e1 :: e2 :: tail)
        }
    }

    loop(List(expr))
  }

  def getSatisfiableTerms(expr: Expr): Set[Expr] = {
    def loop(toDo: List[Expr]): Set[Expr] = toDo match {
      case Nil => throw new Exception("imposible")
      case e :: tail =>
        e match {
          case True | False | Next(True, _) | Next(False, _) =>
            loop(tail)

          case _ if e.isTerm => Set(e)

          case And(es) =>
            val terms = es.filter(_.isTerm)
            if (terms.isEmpty) {
              loop(es.toList ++ tail)
            } else {
              terms
            }
          case Or(es) =>
            loop(es.toList)

          case Eq(e1, e2) =>
            loop(e1 :: e2 :: tail)

          case Impl(e1, e2) =>
            loop(e1 :: e2 :: tail)
        }
    }

    loop(List(expr))
  }

  def getAllTerms(expr: Expr): Set[Expr] = {
    def loop(toDo: List[Expr], acc: Set[Expr]): Set[Expr] = toDo match {
      case Nil => acc
      case e :: tail =>
        e match {
          case True | False | Next(True, _) | Next(False, _) =>
            loop(tail, acc)

          case _ if e.isTerm => e match {
            case Not(x) => loop(tail, acc + x)
            case _ => loop(tail, acc + e)
          }


          case And(es) =>
            loop(es.toList ++ tail, acc)

          case Or(es) =>
            loop(es.toList ++ tail, acc)

          case Eq(e1, e2) =>
            loop(e1 :: e2 :: tail, acc)

          case Impl(e1, e2) =>
            loop(e1 :: e2 :: tail, acc)
        }
    }

    loop(List(expr), Set.empty)
  }

  private def setTrueAnd(es: List[Expr], term: Expr): List[Expr] = es match {
    case Nil => Nil
    case head :: tail =>
      setTrue(head, term) match {
        case True => True :: setTrueAnd(tail, term)
        case False => List(False)
        case e => e :: setTrueAnd(tail, term)
      }
  }

  private def setTrueOr(es: List[Expr], term: Expr): List[Expr] = es match {
    case Nil => Nil
    case head :: tail =>
      setTrue(head, term) match {
        case False => False :: setTrueOr(tail, term)
        case True => List(True)
        case e => e :: setTrueOr(tail, term)
      }
  }

  def setTrue(e: Expr, term: Expr): Expr = e match {
    case And(es) =>
      setTrueAnd(es.toList, term) match {
        case Nil => False
        case e1 :: Nil => e1
        case es1 => LNC.basicSimplify(And(es1.toSet))
      }

    case Or(es) =>
      setTrueOr(es.toList, term) match {
        case Nil => False
        case e1 :: Nil => e1
        case es1 => LNC.basicSimplify(Or(es1.toSet))
      }

    case Eq(e1, e2) =>
      val x1 = setTrue(e1, term) match {
        case x: Not => NormalFormConverter.moveNegInside(x)
        case x => x
      }

      val x2 = setTrue(e2, term) match {
        case x: Not => NormalFormConverter.moveNegInside(x)
        case x => x
      }

      NormalFormConverter.moveNegInside(LNC.basicSimplify(Eq(x1, x2)))

    case _ if e == term =>
      True
    case Var(x) if term == Not(Var(x)) => False
    case Not(Var(x)) if term == Var(x) => False
    case Next(Var(x), l) if term == Not(Next(Var(x), l)) => False
    case Not(Next(Var(x), l)) if term == Next(Var(x), l) => False
    case _ => e
  }

  def isSatisfiable(expr: Expr): Boolean = solveOne(expr).isDefined

  def solveOne(expr: Expr): Option[Expr] = TableAux3(expr).getNext

  def solveAll(expr: Expr): List[Expr] = ???
}


