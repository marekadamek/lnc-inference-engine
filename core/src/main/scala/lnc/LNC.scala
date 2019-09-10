package lnc

import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.expr.ltl.{Always, Finally, Release, Until}

object LNC {

  import NormalFormConverter._
  import lnc.expr.converters.PrefixFormulaConverter._

  /**
    * Calculates depth of the given LNC formula
    *
    * @param formula LNC formula
    * @return depth of LNC formula
    */
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

  /**
    * Reverse temporal direction of input formula by symmetrical replacement of N operator nesting
    *
    * @param e LNC formula
    * @param d depth of input formula
    * @return LNC formula being reversed version of input formula
    */
  def reverse(e: Expr, d: Int): Expr = e match {
    case True | False => e
    case Var(v) => N(d, Var(v))
    case Next(x, l) => reverse(x, d - l)
    case Not(x) => !reverse(x, d)
    case And(es) => Expr.and(es.map(reverse(_, d)))
    case Or(es) => Expr.or(es.map(reverse(_, d)))
    case Impl(e1, e2) => Expr.impl(reverse(e1, d), reverse(e2, d))
    case Eq(e1, e2) => Expr.eq(reverse(e1, d), reverse(e2, d))
  }

  /**
    * Reverse temporal direction of input formula by symmetrical replacement of N operator nesting
    *
    * @param e LNC formula
    * @return LNC formula being reversed version of input formula
    */
  def reverse(e: Expr): Expr = reverse(e, depth(e))

  def calculatePrefixFormula(formula: Expr): Expr = {
    val d = depth(formula)
    val ln = convertToLN(formula)
    val optimized = preprocess(ln, d)
    prefixFormula(optimized)
  }
}
