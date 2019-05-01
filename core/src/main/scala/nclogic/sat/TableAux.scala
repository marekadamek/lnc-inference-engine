package nclogic.sat

import nclogic.model.NormalFormConverter
import nclogic.model.expr._

import scala.collection.mutable


case class TableAux(expr: Expr) {
  private var toDo = List((Set.empty[Expr], Set(expr)))
  private var visited = Set.empty[(Set[Expr], Set[Expr])]

  def isTerm(e: Expr): Boolean = e match {
    case True | False | Var(_) | Not(Var(_)) | Next(Var(_), _) | Not(Next(Var(_), _)) => true
    case _ => false
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

  private def expand(node: (Set[Expr], Set[Expr])): Either[List[(Set[Expr], Set[Expr])], Set[Expr]] = {
    val (terms, exprs) = node
    if (exprs.exists {
      case False | Not(True) | Next(False, _) | Not(Next(True, _)) => true
      case _ => false
    }) {
      return Left(Nil)
    }

    exprs.headOption match {
      case None => Right(terms)
      case Some(e) =>
        val rest = exprs - e
        val nodes = TableAux.setTrue(e, terms) match {
          case False | Not(True) => Nil
          case True | Not(False) => List((terms, rest))

          case t if isTerm(t) =>
            val newTerms = terms + t
            if (isContradictory(newTerms)) {
              Nil
            } else {
              List((newTerms, rest))
            }
          //not not
          case Not(Not(e1)) => List((terms, rest + e1))

          //alpha
          case And(es) => List((terms, rest ++ es))
          case Not(Or(es)) => List((terms, rest ++ es.map(!_)))
          case Not(Impl(e1, e2)) => List((terms, rest + e1 + !e2))
          case Next(Next(e1, l1), l2) => List((terms, rest + Next(e1, l1 + l2)))
          case Next(_, _) => List((terms, rest + NormalFormConverter.moveNInside(e)))
          case Not(Next(e1, l)) =>
            val nInside = NormalFormConverter.moveNInside(Next(e1, l))
            List((terms, rest + Not(nInside)))

          //beta
          case Impl(e1, e2) => List((terms, rest + !e1), (terms, rest + e2))
          case Eq(e1, e2) => List((terms, rest + e1 + e2), (terms, rest + !e1 + !e2))
          case Not(Eq(e1, e2)) => List((terms, rest + e1 + !e2), (terms, rest + !e1 + e2))
          case Or(es) => es.toList.map(x => (terms, rest + x))
          case Not(And(es)) => es.toList.map(x => (terms, rest + !x))
        }

        Left(nodes)
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

object TableAux {

  private def setTrueAnd(es: List[Expr], terms: Set[Expr], acc: Set[Expr] = Set.empty): Expr = es match {
    case Nil if acc.isEmpty => True
    case Nil if acc == Set(True) => True
    case Nil => Expr.and(acc - True)
    case head :: tail =>
      setTrue(head, terms) match {
        case False => False
        case e =>
          if (acc.contains(!e)) False
          else setTrueAnd(tail, terms, acc + e)
      }
  }

  private def setTrueOr(es: List[Expr], terms: Set[Expr], acc: Set[Expr] = Set.empty): Expr = es match {
    case Nil if acc.isEmpty => True
    case Nil if acc == Set(False) => False
    case Nil => Expr.or(acc - False)
    case head :: tail =>
      setTrue(head, terms) match {
        case True => True
        case e =>
          if (acc.contains(!e)) True
          else setTrueOr(tail, terms, acc + e)
      }
  }


  //val cache = mutable.Map.empty[(Expr, Set[Expr]), Expr]

  def setTrue(e: Expr, terms: Set[Expr]): Expr =
   // cache.getOrElseUpdate((e, terms),
      e match {

      case And(es) => setTrueAnd(es.toList, terms)

      case Or(es) => setTrueOr(es.toList, terms)

      case Impl(e1, e2) =>
        setTrue(e1, terms) match {
          case False => True
          case True => setTrue(e2, terms)
          case se1 => setTrue(e2, terms) match {
            case True => True
            case False =>
              NormalFormConverter.moveNegInside(!se1)
            case se2 => Impl(se1, se2)
          }
        }

      case Eq(e1, e2) =>
        (setTrue(e1, terms), setTrue(e2, terms)) match {
          case (x, y) if x == y => True
          case (x, y) if x == !y => False
          case (True, y) => y
          case (False, y) => NormalFormConverter.moveNegInside(!y)
          case (x, True) => x
          case (x, False) => NormalFormConverter.moveNegInside(!x)
          case (x, y) => Eq(x, y)
        }

      case t if t.isTerm && terms.contains(t) => True

      case t if t.isTerm && terms.contains(!t) => False

      case _ if !e.isTerm =>
        e

      case _ => e
    }
 // )



  def isSatisfiable(expr: Expr): Boolean = solveOne(expr).isDefined

  def solveOne(expr: Expr): Option[Set[Expr]] = TableAux(expr).next()

  def solveAll(expr: Expr): List[Set[Expr]] = {
    val tableAux = TableAux(expr)
    var result = List.empty[Set[Expr]]
    var elem = tableAux.next()

    while (elem.isDefined) {
      result = elem.get :: result
      elem = tableAux.next()

    }
    result
  }
}
