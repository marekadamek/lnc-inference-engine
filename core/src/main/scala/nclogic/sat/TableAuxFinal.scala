package nclogic.sat

import nclogic.model.LNC
import nclogic.model.expr._

import scala.collection.mutable

case class TableAuxFinal(formula: Expr, commonBase: Set[Expr] = Set.empty) {
  private var toDo =
    if (commonBase.nonEmpty) {
      commonBase.map(cb => (Set.empty[Expr], Set(formula, cb), List.empty[Expr])).toList
    } else {
      List((Set.empty[Expr], Set(formula), List.empty[Expr]))
    }

  private val visited = mutable.Set.empty[Set[Expr]]

  private def isBaseTerm(e: Expr) = e match {
    case Var(_) | Not(Var(_)) => true
    case _ => false
  }

  private def isNext(e: Expr): Boolean = e match {
    case Next(_, _) => true
    case Not(x) => isNext(x)
    case And(es) => es.forall(isNext)
    case Or(es) => es.forall(isNext)
    case Eq(e1, e2) => isNext(e1) && isNext(e2)
    case _ => false
  }

  private def isAlpha(e: Expr) = e match {
    case _ if isNext(e) => false
    case And(_) => true
    case Not(Or(_)) => true
    case Not(Eq(_, _)) => true
    case _ => false
  }

  private def isBeta(e: Expr) = e match {
    case _ if isNext(e) => false
    case Not(And(_)) | Or(_) => true
    case Eq(_, _) => true
    case _ => false
  }

  private def setTrue(expr: Expr, terms: Set[Expr]): Expr = {
    if (terms.isEmpty) {
      expr
    } else {
      val (t, ts) = (terms.head, terms.tail)

      TableAuxBDD2.setTrue(expr, t) match {
        case False => False
        case True => True
        case e => setTrue(e, ts)
      }
    }
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

  private def getVar(expr: Expr): Option[Expr] = {
    def loop(toDo: List[Expr], not: Boolean): Option[Expr] = toDo match {
      case Nil => None
      case e :: tail =>
        e match {
          case True | False | Next(_, _) => loop(tail, not)

          case v: Var =>
            val x = if (not) Not(v) else v
            Some(x)
          case Not(x) => loop(x :: tail, !not)

          case And(es) =>
            loop(es.toList ++ tail, not)

          case Or(es) =>
            loop(es.toList ++ tail, not)

          case Eq(e1, e2) =>
            loop(e1 :: e2 :: tail, not)

          case Impl(e1, e2) =>
            loop(e1 :: e2 :: tail, not)
        }
    }

    loop(List(expr), not = false)
  }
  def next(): Option[Expr] = {
    var result = Option.empty[Expr]

    while (toDo.nonEmpty && result.isEmpty) {
      val (terms, expressions, path) = toDo.head
      toDo = toDo.tail

      if (!visited.contains(expressions)) {
        visited += expressions

        if (expressions.isEmpty || expressions == Set(True)) {
          val last = if (terms.isEmpty) True else Expr.and(terms)
          result = Some(Expr.and((last :: path).reverse.zipWithIndex.map(p => N(p._2, p._1)).toSet))
        } else if (expressions.contains(False) || Expr.isContradictory(expressions)) {
          //nothing here
        } else {

          getVar(Expr.and(expressions)) match {
            case Some(v) =>
              if (!Expr.isContradictory(terms + v)) {
                val newNode = expressions.map(e => {
                  TableAuxBDD2.setTrue(e, v)
                })
                toDo = (terms + v, newNode, path) :: toDo
              }

            case None =>
              val advanced = expressions.map(advance)
              val newNodes = if (commonBase.isEmpty) {
                List((Set.empty[Expr], advanced, Expr.and(terms) :: path))
              } else {
                commonBase.map(cb => (Set.empty[Expr], advanced + cb, Nil)).toList
              }
              toDo = newNodes ::: toDo

          }
        }
      }
    }

    result
  }

  def solveAll: Set[Expr] = {
    def loop(acc: List[Expr]): List[Expr] = {
      next() match {
        case None => acc
        case Some(e) => loop(e :: acc)
      }
    }

    loop(Nil).toSet
  }
}




