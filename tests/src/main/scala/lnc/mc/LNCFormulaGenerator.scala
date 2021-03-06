package lnc.mc

import lnc.LNC
import lnc.expr._
import lnc.expr.converters.NormalFormConverter

object LNCFormulaGenerator {
  private val rnd = new scala.util.Random

  def generateRandomFormula(vars: List[Var], d: Int, size: Int): Expr = {

    def divide(i: Int): (Int, Int) = {
      if (i <= 0) {
        (0, 0)
      } else {
        val a = rnd.nextInt(i)
        (a, i - a)
      }
    }

    def loop(i: Int): Expr = {
      if (i <= 0) {
        val t = N(rnd.nextInt(d + 1), vars(rnd.nextInt(vars.length)))
        rnd.nextInt(2) match {
          case 0 => t
          case 1 => Expr.not(t)
        }

      }
      else {
        rnd.nextInt(6) match {
          case 0 =>
              C(loop(i - 1))
          case 1 => Expr.not(loop(i - 1))
          case 2 =>
            val (a, b) = divide(i - 1)
            Expr.and(loop(a), loop(b))
          case 3 =>
            val (a, b) = divide(i - 1)
            Expr.or(loop(a), loop(b))
          case 4 =>
            val (a, b) = divide(i - 1)
            Expr.impl(loop(a), loop(b))
          case 5 =>
            val (a, b) = divide(i - 1)
            Expr.eq(loop(a), loop(b))
        }
      }
    }

    def loop2(): Expr = {
      val f = loop(size)
      val e =  NormalFormConverter.moveNextOutside(f, LNC.depth(f)) match {
        case Next(x, _) => NormalFormConverter.convertToNormalForm(x)
        case x => NormalFormConverter.convertToNormalForm(x)
      }

      if (LNC.depth(e) == d) e
      else loop2()
    }

    loop2()

  }

  def generateRandomFormulas(count: Int, vars: List[Var], d: Int, size: Int): List[Expr] = {
    def loop(acc: Set[Expr]): List[Expr] = {
      if (acc.size == count) acc.toList
      else {
        val f = generateRandomFormula(vars, d, size)
        loop(acc + f)
      }
    }

    loop(Set.empty)
  }
}
