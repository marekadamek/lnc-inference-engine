package nclogic.problems

import nclogic.model.HistoryGraph
import nclogic.model.converters.CnfConverter
import nclogic.model.expr.Expr._
import nclogic.model.expr.{C, _}
import nclogic.sat.Sat

object SimpleAlways {

  val policeman = Var("policeman")
  val thief = Var("thief")
  val boat = Var("boat")


  val goRight = (x: Var) => !x & N(x)
  val goLeft = (x: Var) => x & N(!x)

  val problem: Expr = and(
    G(thief <-> policeman)
    , boat -> !(goRight(policeman) | goRight(thief))
    , !boat -> !(goLeft(policeman) | goLeft(thief))
    , C(boat)
    , C(policeman)
    , C(thief) -> C(policeman)

    /////////////////////////////////////////////////
  )

  val start = !boat & !policeman & !thief
  val end = policeman & thief

  def main(args: Array[String]): Unit = {
    val simple = problem.simplify
    val cnf = CnfConverter.convert(problem)
    val sat = Sat.solve(cnf)
    val graph = HistoryGraph(sat)
    val path = graph.findPath(start, end)
    path foreach { x => {
      val es = x.asInstanceOf[And].es
      es.filter(_.isInstanceOf[Neg]).foreach( x => print(x + " "))
      print("  -----   ")
      es.filterNot(_.isInstanceOf[Neg]).foreach( x => print(x + " "))
      println()
    }}
    println(path.size)
  }

}
