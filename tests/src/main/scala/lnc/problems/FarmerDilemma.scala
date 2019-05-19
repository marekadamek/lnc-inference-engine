package lnc.problems

import lnc.expr.Expr.and
import lnc.expr.{C, _}
import lnc.kripke.{KripkeStructureBFS, LNCToKripkeStructureConverter}
import lnc.mc.LNCModel
import lnc.sat.SatSolvers

/**
  * Farmer Dilemma
  *
  * A farmer needs to transport across the river following beings:
  * himself, wolf, sheep and cabbage.
  *
  * Below are the rules of the game:
  * - the raft can take only farmer and at most one more being,
  * - the wolf and sheep cannot be left alone,
  * - sheep and cabbage cannot be left alone,
  */
object FarmerDilemmaProblem {

  /**
    *
    * Variable definition:
    * f - farmer
    * w - wolf
    * s - sheep
    * c - cabbage
    * b - boat
    *
    * If variable is false it means that the related being is on the left side,
    * if it's true the the being is on the right side
    */
  val f = Var("f")
  val w = Var("w")
  val s = Var("s")
  val c = Var("c")

  val problem: Expr = and(
    // always farmer has to be on the same side as wolf
    // or sheep has to be on the opposite side to wolf
    (f <-> w) | (s <-> !w)

    // always farmer has to be on the same side as sheep
    // or cabbage has to be on the opposite side to sheep
    , (f <-> s) | (s <-> !c)

    // position of farmer always changes
    , C(f)

    // wolf, sheep and cabbage cannot pass the river alone
    , (C(w) | C(s) | C(c)) -> C(f)

    // at most two beings can pass the river at the same time
    , C(w) -> (!C(s) & !C(c))
    , C(s) -> (!C(w) & !C(c))
    , C(c) -> (!C(w) & !C(s))

  )

  // every being is on the left side and boat is on the left side
  val from: Set[Expr] = Set[Expr](!f, !w, !s, !c)

  // every being is on the right side
  val to: Set[Expr] = Set[Expr](f, w, s, c)


  def printSolution(path: List[Set[Expr]]): Unit = {
    path.foreach(x => {
      x.filter(_.isInstanceOf[Not]).map(_.asInstanceOf[Not].e).foreach(x => print(x + " "))
      print("  -----   ")
      x.filterNot(_.isInstanceOf[Not]).foreach(x => print(x + " "))
      println()
    })
  }

}

object FarmerDilemma extends App {

  import FarmerDilemmaProblem._

  val (kripke, graphMeasure) = time.measureTime {
    LNCToKripkeStructureConverter.convert(problem, SatSolvers.dpllLike)
  }

  val (path, pathMeasure) = time.measureTime {
    KripkeStructureBFS.findPath(kripke, from, to)
  }

  printSolution(path.get.map(_.terms))

  println()
  println("Execution time (s):")
  println("Building graph: " + graphMeasure.seconds)
  println("Finding solution path: " + pathMeasure.seconds)
  println("TOTAL: " + List(graphMeasure, pathMeasure).map(_.seconds).sum)
}

object FarmerDilemmaSymbolicBFS extends App {

  import FarmerDilemmaProblem._

  val (path, solvingTime) = time.measureTime {
    LNCModel(problem, SatSolvers.dpllLike).findPathBFS(from, to)
  }

  printSolution(path.get)

  println()
  println("Execution time (s): " + solvingTime.seconds)
}

object FarmerDilemmaSymbolicDFS extends App {

  import FarmerDilemmaProblem._

  val (path, solvingTime) = time.measureTime {
    LNCModel(problem, SatSolvers.dpllLike).findPathDFS(from, to)
  }

  printSolution(path.get)

  println()
  println("Execution time (s): " + solvingTime.seconds)
}
