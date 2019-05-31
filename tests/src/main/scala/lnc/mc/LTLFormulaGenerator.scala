package lnc.mc

import lnc.LNC
import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.expr.ltl.{Always, Finally, Release, Until}

object LTLFormulaGenerator {
  private val rnd = new scala.util.Random

  def generateRandomFormula(vars: List[Var], size: Int): Expr = {

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
        rnd.nextInt(8) match {
          case 0 =>
            val l = 1 + rnd.nextInt(i)
            N(loop(i - l))
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
          case 6 =>
            Always(loop(i -1))
          case 7 =>
            Finally(loop(i -1))
//          case 8 =>
//            val (a, b) = divide(i - 1)
//           Until(loop(a), loop(b))
//          case 9 =>
//            val (a, b) = divide(i - 1)
//            Release(loop(a), loop(b))
        }
      }
    }

    loop(size)

  }

  def generateRandomFormulas(count: Int, vars: List[Var], size: Int): List[Expr] = {
    def loop(acc: Set[Expr]): List[Expr] = {
      if (acc.size == count) acc.toList
      else {
        val f = generateRandomFormula(vars, size).simplify
        println("size: " + acc.size)
        loop(acc + f)
      }
    }

    loop(Set.empty)
  }
}
