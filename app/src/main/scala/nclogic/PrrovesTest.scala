package nclogic

import nclogic.model.converters.CnfConverter
import nclogic.model.expr._
import nclogic.sat.Sat

object PrrovesTest extends App {

  val a = Var("a")
  val b = Var("b")
  val c = Var("c")

  val formula = a & !C(a) & C(b) & !C(a -> b)
  //val formula = a -> N(a)

  val cnf = CnfConverter.convert(formula)
  val sat = Sat.solve(cnf)

  println()
  println(cnf)
  println(sat)

}

