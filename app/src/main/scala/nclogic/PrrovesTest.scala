package nclogic

import nclogic.model.expr._
import nclogic.sat.Sat

object PrrovesTest extends App {

  val a = Term("a")
  val b = Term("b")
  val c = Term("c")

  val formula = a & !C(a) & C(b) & !C(a -> b)
  //val formula = a -> N(a)

  val cnf = (formula).cnf
  val sat = Sat.solve(cnf)

  println()
  println(formula.toLatexString)
  println(cnf)
  println(cnf.toLatexString)
  println(sat)

}

