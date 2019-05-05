package lnc

import lnc.expr._
import lnc.expr.converters.{NormalFormConverter, PrefixFormulaConverter}
import lnc.expr.ltl.{Always, Finally, Release, Until}

object LNC {

  def depth(formula: Expr): Int = {

    def loop(elems: List[(Expr, Int)], max: Int): Int = elems match {
      case Nil => max
      case (e, d) :: es => e match {
        case Var(_) | True | False => loop(es, Math.max(max, d))
        case Next(x, l) => loop((x, d + l) :: es, max)
        case Change(x, l) => loop((x, d + l) :: es, max)
        case Not(x) => loop((x, d) :: es, max)
        case Impl(e1, e2) => loop((e1, d) :: (e2, d) :: es, max)
        case Eq(e1, e2) => loop((e1, d) :: (e2, d) :: es, max)
        case And(xs) => loop(xs.map((_, d)).toList ::: es, max)
        case Or(xs) => loop(xs.map((_, d)).toList ::: es, max)

        case Always(x) => loop((x, d) :: es, max)
        case Finally(x) => loop((x, d) :: es, max)
        case Until(e1, e2) => loop((e1, d) :: (e2, d) :: es, max)
        case Release(e1, e2) => loop((e1, d) :: (e2, d) :: es, max)

      }
    }

    loop(List((formula, 0)), 0)
  }

  def reverse(e: Expr, d: Int): Expr = e match {
    case True | False => e
    case Var(v) => N(d, Var(v))
    case Next(x, l) => reverse(x, d - l)
    case Not(x) => !reverse(x, d)
    case And(es) => Expr.and(es.map(reverse(_, d)))
    case Or(es) => Expr.or(es.map(reverse(_, d)))
    case Impl(e1, e2) => Impl(reverse(e1, d), reverse(e2, d))
    case Eq(e1, e2) => Eq(reverse(e1, d), reverse(e2, d))
  }

  def reverse(e: Expr): Expr = reverse(e, depth(e))

  def calculatePrefixFormula(formula: Expr): Expr = {
    val d = LNC.depth(formula)
    val ln = NormalFormConverter.convertToLN(formula)
    val optimized = NormalFormConverter.optimize(ln, d)
    PrefixFormulaConverter.prefixFormula(optimized)
  }
}
