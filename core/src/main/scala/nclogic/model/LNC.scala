package nclogic.model

import nclogic.model.expr._
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
      }
    }

    max
  }


  private def prefix(expr: Expr, depth: Int, cache: mutable.Map[Expr, Option[Expr]]): Expr = {
    if (depth == 0) True
    else {

      def loop(e: Expr): Option[Expr] = {
        cache.getOrElseUpdate(e, {
          e match {
            case True => Some(True)
            case False => Some(False)
            case Next(Var(x), l) => Some(Next(Var(x), l + 1))
            case Not(Next(Var(x), l)) => Some(Not(Next(Var(x), l + 1)))
            case Next(Not(x), l) => loop(Not(Next(x, l)))
            case Var(x) => Some(Next(Var(x), 1))
            case Not(Var(x)) => Some(Not(Next(Var(x), 1)))
            case Not(x) => loop(x).map(Not)

            case And(es) =>
              val converted = es.map(loop).filter(_.isDefined).map(_.get)
              converted.size match {
                case 0 => None
                case 1 => Some(converted.head)
                case _ => Some(And(converted))
              }

            case Or(es) =>
              val converted = es.map(loop).filter(_.isDefined).map(_.get)

              converted.size match {
                case 0 => None
                case 1 => Some(converted.head)
                case _ => Some(Or(converted))
              }

            case Eq(e1, e2) =>
              val ce1 = loop(e1)
              val ce2 = loop(e2)
              if (ce1.isEmpty || ce2.isEmpty) {
                None
              } else {
                Some(Eq(ce1.get, ce2.get))
              }

            case Impl(e1, e2) => loop(Or(NormalFormConverter.moveNegInside(Not(e1)), e2))

          }
        })
      }

      loop(expr).getOrElse(True)
    }
  }

  private def cutToDepth(expr: Expr, depth: Int): Expr = {
    val cache = mutable.HashMap.empty[Expr, Option[Expr]]

    def isOut(e: Expr): Boolean = e match {
      case Next(_, l) => l > depth
      case Not(Next(_, l)) => l > depth
      case _ => false
    }

    def loop(e: Expr): Option[Expr] = {
      cache.getOrElseUpdate(e, {
        e match {
          case True => Some(True)
          case False => Some(False)
          case Next(Var(x), l) => if (l <= depth) Some(e) else None
          case Not(Next(Var(x), l)) => if (l <= depth) Some(e) else None
          case Var(_) | Not(Var(_)) => Some(e)

          case And(es) =>
            val (outs, rest) = es.partition(isOut)
            val converted = rest.map(TableAux.setTrue(_, outs)).map(loop).filter(_.isDefined).map(_.get)
            converted.size match {
              case 0 => None
              case 1 => Some(converted.head)
              case _ => Some(And(converted))
            }

          case Or(es) =>
            val (outs, rest) = es.partition(isOut)
            val converted = rest.diff(outs).map(loop).filter(_.isDefined).map(_.get)
            converted.size match {
              case 0 => None
              case 1 => Some(converted.head)
              case _ => Some(Or(converted))
            }

          case Eq(e1, e2) =>
            val se1 = loop(e1)
            val se2 = loop(e2)
            if (se1.isEmpty || se2.isEmpty)
              None
            else {
              Some(Eq(loop(e1).get, loop(e2).get))
            }
          //
          //          case Impl(e1, e2) => Some(loop(Or(NormalFormConverter.moveNegInside(Not(e1)), e2)))
          //
        }
      })
    }

    loop(expr).getOrElse(True)
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
            case x1 => Not(x1)
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

          case Impl(e1, e2) => loop(Or(Not(e1), e2))

          case Eq(e1, e2) =>
            loop(e1) match {
              case True => loop(e2)
              case False => loop(Not(e2))
              case s1 =>
                loop(e2) match {
                  case True => s1
                  case False => loop(Not(s1))
                  case s2 => Eq(s1, s2)
                }
            }

          case Next(True, _) => True
          case Next(False, _) => False

          case Next(x, l) => Next(loop(x), l)

          case Change(_, _) => ???
        }
      })
    }

    loop(expr)
  }


  def simplify(expr: Expr): Expr = {
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
            case x1 => Not(x1)
          }

          case And(es) =>
            val eqs = es.filter(_.isInstanceOf[Or])

            val x = eqs.map {
              case Eq(e1, e2) if es.contains(e1) => e2
              case Eq(e1, e2) if es.contains(e2) => e1
              case Eq(e1, e2) if es.exists(e => Expr.isContradictory(Set(e, e1))) => Not(e2)
              case Eq(e1, e2) if es.exists(e => Expr.isContradictory(Set(e, e2))) => Not(e1)
              case eq => eq
            }

            val pre = es -- eqs ++ x

            val simplified = pre.map(loop).foldLeft(Set.empty[Expr]) {
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
                case _ =>
                  val (terms, nonTerms) = simplified.partition(_.isTerm)
                  val termsApplied = nonTerms.map(TableAux.setTrue(_, terms))
                  if (nonTerms == termsApplied) {
                    And(simplified)
                  } else {
                    loop(And(terms ++ termsApplied))
                  }
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

          case Impl(e1, e2) => loop(Or(Not(e1), e2))

          case Eq(e1, e2) =>
            loop(e1) match {
              case True => loop(e2)
              case False => loop(Not(e2))
              case s1 =>
                loop(e2) match {
                  case True => s1
                  case False => loop(Not(s1))
                  case s2 => Eq(s1, s2)
                }
            }

          case Next(True, _) => True
          case Next(False, _) => False

          case Next(x, l) => Next(loop(x), l)

          case Change(_, _) => ???
        }
      }
      )
    }

    loop(expr)
  }
}
