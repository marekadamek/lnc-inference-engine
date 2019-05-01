package nclogic.model

import nclogic.model.expr._
import nclogic.model.expr.ltl.{Always, Finally, Release, Until}
import nclogic.sat.TableAux

import scala.collection.mutable

object LNC {

  def depth(formula: Expr): Int = {

    var max = 0
    var elements = List((formula, 0))

    while (elements.nonEmpty) {
      val ((e, d), tail) = (elements.head, elements.tail)

      e match {
        case Var(_) | True | False =>
          max = Math.max(max, d)
          elements = tail

        case Next(x, l) =>
          elements = (x, d + l) :: tail

        case Change(x, l) =>
          elements = (x, d + l) :: tail

        case Not(x) =>
          elements = (x, d) :: tail

        case Impl(e1, e2) =>
          elements = (e1, d) :: (e2, d) :: tail

        case Eq(e1, e2) =>
          elements = (e1, d) :: (e2, d) :: tail

        case And(es) =>
          elements = es.map((_, d)).toList ++ tail

        case Or(es) =>
          elements = es.map((_, d)).toList ++ tail

        case Always(x) =>
          elements = (x, d) :: tail

        case Finally(x) =>
          elements = (x, d) :: tail

        case Until(e1, e2) =>
          elements = (e1, d) :: (e2, d) :: tail

        case Release(e1, e2) =>
          elements = (e1, d) :: (e2, d) :: tail
      }
    }

    max
  }


  def suffix(e: Expr): Expr = {
    val result = e match {
      case Var(_) | Not(Var(_)) | True | False => True
      case Next(x, l) => if (l == 1) x else Next(x, l - 1)
      case Not(Next(x, l)) => if (l == 1) Not(x) else Not(Next(x, l - 1))
      case And(es) => And(es.map(suffix))
      case Or(es) => Or(es.map(suffix))
    }

    basicSimplify(result)
  }

  def prefixFormula(e: Expr): Expr = {
    NormalFormConverter.convert(e)
  }

  def prefixFormula2(e: Expr): Expr = {
    val d = depth(e)
    val base = NormalFormConverter.convertToLN(e)
    val prefixes = (1 to d) map { i => N(i, base) }

    And(prefixes.toSet + base)
  }

  def stripTemporalFromAnd(e: Expr): Expr = e match {
    case And(es) => And(es.map(stripTemporalFromAnd)).simplify
    case Next(_, _) | Not(Next(_, _)) => True
    case Var(_) | Not(Var(_)) => e
    case _ => throw new Exception("Should not happen")
  }


  def basicSimplify(expr: Expr): Expr = {
    val cache = mutable.HashMap.empty[Expr, Expr]

    def loop(e: Expr): Expr = {
      cache.getOrElseUpdate(e, {
        e match {
          case True | False => e
          case x if x.isTerm => x
          case Not(True) => False
          case Not(False) => True
          case Not(Not(x)) => loop(x)
          case Not(x) => loop(x) match {
            case True => False
            case False => True
            case x1 => NormalFormConverter.moveNegInside(!x1)
          }

          case And(es) =>
            val simplified = es.map(loop).foldLeft(Set.empty[Expr]) {
              case (set, And(e)) => set ++ e
              case (set, True) => set
              case (set, Next(True, _)) => set
              case (set, e) => set + e
            }

            val containsFalse = simplified.exists {
              case False => true
              case Next(False, _) => true
              case _ => false
            }

            if (containsFalse || Expr.isContradictory(simplified))
              False
            else {
              simplified.size match {
                case 0 => True
                case 1 => simplified.head
                case _ => And(simplified)
              }
            }

          case Or(es) =>
            val simplified = es.map(loop).foldLeft(Set.empty[Expr]) {
              case (set, Or(e)) => set ++ e
              case (set, False) => set
              case (set, Next(False, _)) => set
              case (set, e) => set + e
            }

            val containsTrue = simplified.exists {
              case True => true
              case Next(True, _) => true
              case _ => false
            }

            if (containsTrue || Expr.isContradictory(simplified))
              True
            else {
              simplified.size match {
                case 0 => False
                case 1 => simplified.head
                case _ => Or(simplified)
              }
            }

          case Impl(e1, e2) =>
            loop(e1) match {
              case False => True
              case True => loop(e2)
              case se1 => loop(e2) match {
                case True => True
                case False => NormalFormConverter.moveNegInside(!se1)
                case se2 => Impl(se1, se2)
              }
            }

          case Eq(e1, e2) =>
            loop(e1) match {
              case True => loop(e2)
              case False => loop(Not(e2))
              case s1 =>
                loop(e2) match {
                  case True => s1
                  case False => loop(Not(s1))
                  case s2 => (s1, s2) match {
                    case _ if s1 == s2 => True
                    case _ if s1 == !s2 => False
                    case (Not(x), Not(y)) => Eq(x, y)
                    case _ => Eq(s1, s2)
                  }
                }
            }

          case Next(True, _) => True
          case Next(False, _) => False

          case Next(Next(x, l), l1) => Next(loop(x), l + l1)

          case Next(x, l) => Next(loop(x), l)

          case Change(Next(x, l1), l) => loop(Next(Change(x, l), l1))

          case Change(x, l) => Change(loop(x), l)
        }
      })
    }

    loop(expr)
  }
}
