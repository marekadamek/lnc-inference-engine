package nclogic.sat

import nclogic.model.LNC
import nclogic.model.expr._

import scala.collection.mutable

case class Node3(terms: Set[Expr], e: Expr)

case class TableAuxBDD(formula: Expr) {
  val depth = LNC.depth(formula)
  val graph = new Graph[Expr, Set[Set[Expr]]]
  var visited = mutable.Set.empty[Expr]
  var toDo = List((List.empty[Set[Expr]], formula))

  val solutions = mutable.Set.empty[Expr]

  private def getVar(expr: Expr): Option[Var] = {
    def loop(toDo: List[Expr]): Option[Var] = toDo match {
      case Nil => None
      case e :: tail =>
        e match {
          case True | False | Next(_, _) => loop(tail)

          case v: Var => Some(v)

          case Not(x) => loop(x :: tail)

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

  private def advance(expr: Expr): Expr = {
    def loop(e: Expr): Expr = e match {
      case False => False

      case True | Var(_) | Not(Var(_)) => True

      case Next(x, l) => N(l - 1, x)

      case Not(x) => Not(loop(x))

      case And(es) =>
        And(es.map(loop))

      case Or(es) =>
        Or(es.map(loop))

      case Eq(e1, e2) =>
        Eq(loop(e1), loop(e2))

    }

    LNC.basicSimplify(loop(expr))
  }

  def solveAll(expr: Expr): Map[Expr, Set[Set[Expr]]] = {
    var nodes = List((Set.empty[Expr], expr))

    val results = mutable.Map.empty[Expr, Set[Set[Expr]]]

    while (nodes.nonEmpty) {
      val (terms, parent) = nodes.head
      nodes = nodes.tail

      parent match {
        case False =>
        case True =>
          val solutions = results.getOrElseUpdate(parent, Set.empty)
          results.put(parent, solutions + terms)

        case _ =>
          getVar(parent) match {
            case None =>
              val solutions = results.getOrElseUpdate(parent, Set.empty)
              results.put(parent, solutions + terms)

            case Some(v) =>
              val e1 = TableAuxBDD.setTrue(parent, v)
              val e2 = TableAuxBDD.setTrue(parent, Not(v))
              nodes = (terms + v, e1) :: (terms + Not(v), e2) :: nodes
          }
      }


    }
    results.toMap
  }

  def expand(parent: Expr): Unit = {
    if (!visited(parent)) {
      visited += parent

      if (parent == True) {
        println()
      }

      val all = solveAll(parent)


      all.foreach {
        case (e, edges) =>
          graph.addEdge(parent, advance(e), edges)
      }
    }
  }


  val visited2 = mutable.Set.empty[Expr]

  private def calculateNextBase(): Option[Expr] = {
    var result = Option.empty[Expr]

    while (toDo.nonEmpty && result.isEmpty) {
      val (path, parent) = toDo.head
      toDo = toDo.tail

      if (!visited2.contains(parent)) {
        visited2 += parent

        parent match {
          case True =>
            result = Some(Expr.and(path.reverse.zipWithIndex.map {
              case (set, i) => N(i, Expr.and(set))
            }.toSet))
          case _ =>
            expand(parent)

            for {
              (child, solutions) <- graph.getSuccessors(parent)
            } {
              toDo = (solutions.head :: path, child) :: toDo
            }
        }
      }
    }

    result
  }


  val visitedPrefixes = mutable.Map.empty[(Expr, Expr), Boolean]

  def prefix(expr: Expr, d: Int): Expr = {

    def loop(e: Expr): Expr = e match {
      case True => True
      case Next(_, l) if l >= d => True
      case Next(x, l) => Next(x, l + 1)
      case And(es) => Expr.and(es.map(prefix(_, d)))
      case _ => Next(e, 1)
    }

    LNC.basicSimplify(loop(expr))
  }

  private val successorsMap = mutable.Map.empty[Expr, Set[Expr]]

  private def isVar(e: Expr) = e match {
    case Var(_) | Not(Var(_)) => true
    case _ => false
  }

  private def getSuccessors(expr: Expr): Set[Expr] = {
    successorsMap.getOrElseUpdate(expr, {

      val suffix = LNC.suffix(expr)
      var toDo = List((formula, suffix, 0))

      val lastTerms = mutable.Set.empty[Expr]

      while (toDo.nonEmpty) {
        val (parent, current, l) = toDo.head
        toDo = toDo.tail

        expand(parent)

        val next = if (l == depth) {
          lastTerms ++= graph.getSuccessors(parent).map(_._2.head).map(set => N(depth, Expr.and(set)))
          Nil
        } else {
          val vars = current match {
            case True | Next(_, _) => Set.empty
            case _ if isVar(current) => Set(current)
            case And(es) => es.filter(isVar)
          }

          graph.getSuccessors(parent).filter(_._2.exists(e => !Expr.isContradictory(e ++ vars))).map(_._1)
        }
        val advanced = advance(current)
        toDo = next.map(n => (n, advanced, l + 1)) ::: toDo
      }


      lastTerms.map(lt => Expr.and(suffix, lt)).toSet
    })
  }

  def checkSuffixes(base: Expr): Boolean = {
      var toDo = List((List(base), 0))
      var ok = false

      while (toDo.nonEmpty && !ok) {
        val (path, i) = toDo.head
        toDo = toDo.tail

        if (i == depth || path.tail.contains(path.head)) {
          ok = true
        } else {
          toDo = getSuccessors(path.head).toList.map(next => (next :: path, i + 1)) ::: toDo
        }
      }

      ok
  }

  def getNext: Option[Expr] = {
    var base = calculateNextBase()
    var found = false

    while (base.nonEmpty && !found) {
      found = checkSuffixes(base.get)
      if (!found) {
        base = calculateNextBase()
      }
    }

    base
  }

}

object TableAuxBDD {

  private def setTrueAnd(es: List[Expr], term: Expr, acc: Set[Expr]): Expr = {
    es match {
      case Nil => if (acc.isEmpty) True else Expr.and(acc)
      case head :: tail =>
        setTrue(head, term) match {
          case True => setTrueAnd(tail, term, acc)
          case False => setTrueAnd(Nil, term, Set(False))
          case e if acc.contains(!e) => setTrueAnd(Nil, term, Set(False))
          case e => setTrueAnd(tail, term, acc + e)
        }
    }
  }

  private def setTrueOr(es: List[Expr], term: Expr, acc: Set[Expr]): Expr = {
    es match {
      case Nil => if (acc.isEmpty) False else Expr.or(acc)
      case head :: tail =>
        setTrue(head, term) match {
          case False => setTrueOr(tail, term, acc)
          case True => setTrueOr(Nil, term, Set(True))
          case e if acc.contains(!e) => setTrueOr(Nil, term, Set(True))
          case e => setTrueOr(tail, term, acc + e)
        }
    }
  }

  def setTrue(e: Expr, vs: Set[Expr]): Expr = {
    var current = e
    vs.foreach(v => {
      current = setTrue(current, v)
    })
    current
  }

  def setTrue(e: Expr, v: Expr): Expr = {
    val result = e match {
      case True => True
      case False => False

      case Var(x) =>
        v match {
          case Var(x1) if x == x1 => True
          case Not(Var(x1)) if x == x1 => False
          case _ => e
        }
      case Not(Var(x)) =>
        v match {
          case Not(Var(x1)) if x == x1 => True
          case Var(x1) if x == x1 => False
          case _ => e
        }

      case Not(x) =>
        setTrue(x, v) match {
          case True => False
          case False => True
          case Not(x1) => x1
          case x1 => Not(x1)
        }

      case Next(_, _) => e

      case And(es) =>
        setTrueAnd(es.toList, v, Set.empty)

      case Or(es) =>
        setTrueOr(es.toList, v, Set.empty)

      case Eq(e1, e2) =>
        (setTrue(e1, v), setTrue(e2, v)) match {
          case (True, x2) => x2
          case (False, x2) => !x2
          case (x1, True) => x1
          case (x1, False) => !x1
          case (x1, x2) => Eq(x1, x2)
        }
    }

    result
  }


  def isSatisfiable(expr: Expr): Boolean = solveOne(expr).isDefined

  def solveOne(expr: Expr): Option[Expr] = TableAuxBDD(expr).getNext

  def solveAll(expr: Expr): List[Expr] = ???
}


