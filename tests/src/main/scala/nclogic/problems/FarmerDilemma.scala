package nclogic.problems

import nclogic.model.HistoryGraph
import nclogic.model.converters.CnfConverter
import nclogic.java.model.expr.Expr.and
import nclogic.java.model.expr.{C, _}
import nclogic.sat.Sat

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
object FarmerDilemma {

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

  def goesRight(x: Var): Expr = !x & N(x)
  def goesLeft(x: Var): Expr = x & N(!x)

  val problem: Expr = and(
    // always farmer has to be on the same side as wolf
    // or sheep has to be on the opposite side to wolf
    G((f <-> w) | (s <-> !w))

    // always farmer has to be on the same side as sheep
    // or cabbage has to be on the opposite side to sheep
    , G((f <-> s) | (s <-> !c))

    // if farmer is on the right side then nobody goes right
    , f -> !Or.formSet(Set(w, s, c).map(goesRight))

    // if farmer is on the left side then nobody goes left
    , !f -> !Or.formSet(Set(w, s, c).map(goesLeft))

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
  val from = !f & !w & !s & !c

  // every being is on the right side
  val to = f & w & s & c

  def main(args: Array[String]): Unit = {
    val cnfMeasure = time.measureTime {
      CnfConverter.convert(problem)
    }

    val satMeasure = time.measureTime {
      Sat.solve(cnfMeasure.result)
    }

    val graphMeasure = time.measureTime {
      HistoryGraph(satMeasure.result)
    }

    val pathMeasure = time.measureTime {
      graphMeasure.result.findPath(from, to)
    }

    pathMeasure.result.foreach(x => {
      val es = x.asInstanceOf[And].es
      es.filter(_.isInstanceOf[Neg]).map(_.asInstanceOf[Neg].e).foreach(x => print(x + " "))
      print("  -----   ")
      es.filterNot(_.isInstanceOf[Neg]).foreach(x => print(x + " "))
      println()
    })

    println()
    println("Execution time (ms):")
    println("Calculating CNF: " + cnfMeasure.millis)
    println("Solving SAT: " + satMeasure.millis)
    println("Building graph: " + graphMeasure.millis)
    println("Finding solution path: " + pathMeasure.millis)
    println("TOTAL: " + List(cnfMeasure, satMeasure, graphMeasure, pathMeasure).map(_.millis).sum)
  }

}
