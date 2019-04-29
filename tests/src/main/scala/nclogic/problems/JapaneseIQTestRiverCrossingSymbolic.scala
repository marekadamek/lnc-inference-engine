package nclogic.problems

import kripke.LNCToKripkeStructureConverter
import nclogic.model.expr._
import nclogic.sat.SatSolvers

object JapaneseIQTestRiverCrossingSymbolic {

  val problem: Expr = JapaneseIQTestRiverCrossing.problem
  val from = JapaneseIQTestRiverCrossing.from
  val to = JapaneseIQTestRiverCrossing.to

  def main(args: Array[String]): Unit = {
    val (path1, time2) = time.measureTime {
      LNCToKripkeStructureConverter.findPathDFS(problem, from, to,  SatSolvers.dpllLike)
    }

    path1.get.foreach(x => {
      x.filter(_.isInstanceOf[Not]).map(_.asInstanceOf[Not].e.asInstanceOf[Var]).toList.sortBy(_.name).foreach(x => print(x + " "))
      print("  -----   ")
      x.filter(_.isInstanceOf[Var]).map(_.asInstanceOf[Var]).toList.sortBy(_.name).foreach(x => print(x + " "))
      println()
    })

    println()
    println("Execution time (s):")
    println("TOTAL: " + List(time2).map(_.seconds).sum)
  }

}
