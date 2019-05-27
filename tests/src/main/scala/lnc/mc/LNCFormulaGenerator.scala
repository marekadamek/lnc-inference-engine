package lnc.mc

import lnc.LNC
import lnc.expr._
import lnc.expr.converters.NormalFormConverter

object LNCFormulaGenerator {
  private val rnd = new scala.util.Random

  def generateRandomFormula(vars: List[Var], d: Int, size: Int): Expr = {

    def divide(i: Int): (Int, Int) = {
      if (i <= 0) {
        (0,0)
      } else {
        val a = rnd.nextInt(i)
        (a, i - a)
      }
    }

    def loop(i: Int): Expr = {
      if (i <= 0) {
        vars(rnd.nextInt(vars.length))
      }
      else {
        rnd.nextInt(7) match {
          case 0 =>
            val l = 1 + rnd.nextInt(i)
            N(loop(i - l))
          case 1 =>
            val l = 1 + rnd.nextInt(i)
            if (2*l <= 1) {
              C(loop(i - l * 2))
            } else {
              loop(i)
            }
          case 2 => Expr.not(loop(i - 1))
          case 3 =>
            val (a, b) = divide(i - 1)
            Expr.and(loop(a), loop(b))
          case 4 =>
            val (a, b) = divide(i - 1)
            Expr.or(loop(a), loop(b))
          case 5 =>
            val (a, b) = divide(i - 1)
            Expr.impl(loop(a), loop(b))
          case 6 =>
            val (a, b) = divide(i - 1)
            Expr.eq(loop(a), loop(b))
        }
      }
    }

    def loop2(): Expr = {
      val x = NormalFormConverter.convertToNormalForm(loop(size))
      x match {
        case e if LNC.depth(e) == d =>
          e
        case _ =>
          loop2()
      }
    }

    loop2()

  }

  def generateRandomFormulas(count: Int, vars: List[Var], d: Int, size: Int): List[Expr] = {
    def loop(acc: Set[Expr]): List[Expr] = {
      if (acc.size == count) acc.toList
      else {
        val f = generateRandomFormula(vars, d, size)
        println("size: " + acc.size)
        loop(acc + f)
      }
    }

    loop(Set.empty)
  }
}
