package nclogic.sat

import nclogic.model.LNC
import nclogic.model.expr._

import scala.collection.mutable

case class TableAuxBDD2(formula: Expr) {
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
              val e1 = TableAux.setTrue(parent, Set(v))
              val e2 = TableAux.setTrue(parent, Set(Not(v)))
              nodes = (terms + v, e1) :: (terms + Not(v), e2) :: nodes
          }
      }


    }
    results.toMap
  }

  def expand(parent: Expr): Unit = {
    if (!visited(parent)) {
      visited += parent

      val all = solveAll(parent)


      all.foreach {
        case (e, edges) =>
          graph.addEdge(parent, advance(e) & formula, edges)
      }
    }
  }


  val visited2 = mutable.Set.empty[Expr]

  def next(): Option[Expr] = {
    var result = Option.empty[Expr]

    while (toDo.nonEmpty && result.isEmpty) {
      val (path, parent) = toDo.head
      toDo = toDo.tail

      if (!visited2.contains(parent)) {
        visited2 += parent

        if (parent == True)
          result = Some(Expr.and(path.reverse.zipWithIndex.map {
            case (set, i) => N(i, Expr.and(set))
          }.toSet))

        else if (path.length == depth + 1) {
          val all = solveAll(parent)

          result =Some(True)

        } else {
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

}

object TableAuxBDD2 {

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

  def memoize[I, O](f: I => O): I => O = {
    val cache = new mutable.HashMap[I, O]()
    (key: I) => cache.getOrElseUpdate(key, f(key))
  }

  val setTrue: ((Expr, Expr)) => Expr = memoize {
    case (e: Expr, v: Expr) => e match {
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
  }


  def isSatisfiable(expr: Expr): Boolean = solveOne(expr).isDefined

  def solveOne(expr: Expr): Option[Expr] = TableAuxBDD2(expr).next()

  def solveAll(expr: Expr): Set[Expr] = {
    val tableAux = TableAuxBDD2(expr)

    def loop(acc: List[Expr]): List[Expr] = {
      tableAux.next() match {
        case None => acc
        case Some(e) => loop(e :: acc)
      }
    }

    loop(Nil).toSet
  }
}


