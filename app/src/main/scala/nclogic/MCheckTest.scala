package nclogic

import nclogic.mcheck.MCheckConverter
import nclogic.model.HistoryGraph
import nclogic.model.converters.CnfConverter
import nclogic.model.expr._
import nclogic.sat.Sat

object MCheckTest extends App {

  val a = Var("a")
  val b = Var("b")

  val formula = C(C(a | b))

  val sat = Sat.solve(CnfConverter.convert(formula))
  val graph = HistoryGraph(sat)

  val converted1 = MCheckConverter.convert1(graph)
  val converted2 = MCheckConverter.convert2(graph)

  println(converted2)

}

