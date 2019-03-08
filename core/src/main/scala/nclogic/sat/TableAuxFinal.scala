package nclogic.sat

import nclogic.model.LNC
import nclogic.model.expr._

import scala.collection.mutable

case class TableAuxFinal(formula: Expr, commonBase: Set[Expr] = Set.empty) {
  private var toDo =
    if (commonBase.nonEmpty) {
      commonBase.map(cb => (Set.empty[Expr], Set(formula, cb))).toList
    } else {
      List((Set.empty[Expr], Set(formula)))
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

    loop(List(expr), false)
  }
  def next(): Option[Expr] = {
    var result = Option.empty[Expr]

    while (toDo.nonEmpty && result.isEmpty) {
      val (terms, expressions) = toDo.head
      toDo = toDo.tail

      if (!visited.contains(expressions)) {
        visited += expressions

        if (expressions.isEmpty || expressions == Set(True)) {
          result = Some(if (terms.isEmpty) True else Expr.and(terms))
        } else if (expressions.contains(False) || Expr.isContradictory(expressions)) {
          //nothing here
        } else {

          getVar(And(expressions)) match {
            case Some(v) =>
              if (!Expr.isContradictory(terms + v)) {
                val newNode = expressions.map(e => TableAuxBDD2.setTrue(e, v))
                toDo = (terms + v, newNode) :: toDo
              }

            case None =>
              val advanced = expressions.map(advance)
              val newNodes = if (commonBase.isEmpty) {
                List((Set.empty[Expr], advanced))
              } else {
                commonBase.map(cb => (Set.empty[Expr], advanced + cb)).toList
              }
              toDo = newNodes ::: toDo

          }
//          val (newTerms, nonTerms) = expressions.partition(isBaseTerm)
//
//          //terms
//          if (newTerms.nonEmpty) {
//            if (!Expr.isContradictory(terms ++ newTerms)) {
//              val newNode = nonTerms.map(e => setTrue(e, newTerms))
//              toDo = (terms ++ newTerms, nonTerms ++ newNode) :: toDo
//            }
//          } else {
//            val (alphas, nonAlphas) = expressions.partition(isAlpha)
//
//            if (alphas.nonEmpty) {
//              val postAlphas = alphas.flatMap {
//                case And(es) => es
//                case Not(Or(es)) => es.map(!_)
//                case Not(Eq(e1, e2)) => Set[Expr](e1 | e2, !e1 | !e2)
//              }
//
//              val newNode = (terms, postAlphas ++ nonAlphas)
//              toDo = newNode :: toDo
//            } else {
//              expressions.find(isBeta) match {
//                case Some(beta) =>
//                  val postBeta = beta match {
//                    case Not(And(es)) => es.map(!_)
//                    case Or(es) => es
//                    case Eq(e1, e2) => Set(e1 & e2, !e1 & !e2)
//                  }
//
//                  val newNodes = postBeta.map(pb => (terms, expressions - beta + pb))
//                  toDo = newNodes.toList ::: toDo
//
//                case None =>
//                  val advanced = expressions.map(advance)
//                  val newNodes = commonBase.map(cb => (Set.empty[Expr], advanced + cb))
//
//                  toDo = newNodes.toList ::: toDo
//              }
//            }
//          }

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




