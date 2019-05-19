package lnc.sat

import lnc.bool.BoolSatIterator
import lnc.expr._

case class TableAux(expr: Expr) extends BoolSatIterator {
  private var toDo = List((Set.empty[Expr], List(expr)))

  private def expand(node: (Set[Expr], List[Expr])): Either[List[(Set[Expr], List[Expr])], Set[Expr]] = {
    val (terms, exprs) = node

    exprs match {
      case Nil => Right(terms)
      case e :: rest =>
        Expr.setTrue(e, terms) match {
          case False | Not(True) => Left(Nil)
          case True | Not(False) => Left(List((terms, rest)))

          case t if Expr.isTerm(t) =>
            if (terms.contains(t)) {
              Left(List((terms, rest)))
            } else if (Expr.isContradictory(t, terms)) {
              Left(Nil)
            } else {
              Left(List((terms + t, rest)))
            }

          case _ =>

            val nodes: List[List[Expr]] = e match {
              //not not
              case Not(Not(e1)) => List(
                List(e1)
              )

              case Not(Next(Not(x), l)) => List(
                List(Next(x, l))
              )

              case Not(Next(Next(x, l), l1)) => List(
                List(Not(Next(x, l + l1)))
              )

              //alpha
              case Next(Not(x), l) => List(
                List(Not(Next(x, l)))
              )

              case And(es) => List(
                es.toList
              )

              case Not(Or(es)) => List(
                es.map(!_).toList
              )

              case Not(Impl(e1, e2)) => List(
                List(e1, !e2)
              )

              case Next(Next(e1, l1), l2) => List(
                List(Next(e1, l1 + l2))
              )

              case Next(And(es), l) => List(
                es.map(Next(_, l)).toList
              )

              case Not(Next(Or(es), l)) => List(
                es.map(!Next(_, l)).toList
              )

              case Not(Next(Impl(e1, e2), l)) => List(
                List(Next(e1, l), !Next(e2, l))
              )

              case Not(Eq(e1, e2)) => List(
                List(Or(e1, e2), Or(!e1, !e2))
              )

              //beta
              case Or(es) => es.toList.map(List(_))

              case Not(And(es)) => es.toList.map(e => List(!e))

              case Impl(e1, e2) => List(
                List(!e1), List(e2)
              )

              case Eq(e1, e2) => List(
                List(e1, e2), List(!e1, !e2)
              )

              case Next(Or(es), l) => es.toList.map(e => List(N(l, e)))

              case Not(Next(And(es), l)) => es.toList.map(e => List(!Next(e, l)))

              case Next(Impl(e1, e2), l) => List(
                List(!Next(e1, l)), List(Next(e2, l))
              )
              case Next(Eq(e1, e2), l) => List(
                List(Next(e1, l), Next(e2, l)), List(!Next(e1, l), !Next(e2, l))
              )

              case Not(Next(Eq(e1, e2), l)) => List(
                List(Next(e1, l), !Next(e2, l)), List(!Next(e1, l), !Next(e2, l))
              )
            }

            Left(nodes.map(n => (terms, n ::: rest)))
        }
    }
  }

  def next(): Option[Set[Expr]] = {
    var result = Option.empty[Set[Expr]]

    while (toDo.nonEmpty && result.isEmpty) {
      val node = toDo.head
      toDo = toDo.tail

      expand(node) match {
        case Right(solution) =>
          result = Some(solution)
        case Left(next) =>
          toDo = next ::: toDo
      }
    }

    result
  }
}
