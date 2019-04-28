package nclogic.problems

import kripke.LNCToKripkeStructureConverter
import nclogic.model.expr._
import nclogic.sat.SatSolvers


object FarmerDilemmaSymbolic {

  val problem: Expr = FarmerDilemma.problem
  val from = FarmerDilemma.from
  val to = FarmerDilemma.to

  def main(args: Array[String]): Unit = {

    val (path1, time2) = time.measureTime {
      LNCToKripkeStructureConverter.findPathBFS(problem, from, to, SatSolvers.miniSat)
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
