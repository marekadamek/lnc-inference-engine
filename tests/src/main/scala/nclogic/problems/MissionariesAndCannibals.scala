package nclogic.problems

import nclogic.model.HistoryGraph
import nclogic.model.converters.CnfConverter
import nclogic.java.model.expr.Expr._
import nclogic.java.model.expr.{C, _}
import nclogic.sat.Sat

/**
  * Three missionaries and three cannibals must cross a river using a boat which
  * can carry at most two people, under the constraint that, for both banks,
  * if there are missionaries present on the bank, they cannot be outnumbered
  * by cannibals (if they were, the cannibals would eat the missionaries).
  * The boat cannot cross the river by itself with no people on board.
  */
object MissionariesAndCannibals {

  /**
    * mx - true means that x missionaries are one the right bank of the river,
    * cx - true means that x cannibals are one the right bank of the river,
    * b - if true means that the boat is on the right side.
    */
  val m0 = Var("m0")
  val m1 = Var("m1")
  val m2 = Var("m2")
  val m3 = Var("m3")
  val c0 = Var("c0")
  val c1 = Var("c1")
  val c2 = Var("c2")
  val c3 = Var("c3")
  val b = Var("b")

  /**
    * The code below generate disjunction of conjunctions in which exactly one variable is true
    *
    * eg. for x1, x2, x3 it generates:
    * (x1 & !x2 & !x3) | (!x1 & x2 & !x3) | (!x1 & !x2 & x3)
    */
  def onlyOneTrue(vars: Set[Expr]): Expr = {
    val ands = vars.map(v => {
      val rest = vars - v
      And(rest.map(_.not) + v)
    })
    Or.formSet(ands)
  }

  val missionariesAreMoving = C(m0) | C(m1) | C(m2) | C(m3)
  val cannibalsAreMoving = C(c0) | C(c1) | C(c2) | C(c3)

  val problem: Expr = and(
    // always only one of mx and one of cx can be true
    G(onlyOneTrue(Set(m0, m1, m2, m3)) & onlyOneTrue(Set(c0, c1, c2, c3)))

    // cannibals cannot outnumber missionaries on the right side
    , G((m1 -> !(c2 | c3)) & (m2 -> !c3))

    // cannibals cannot outnumber missionaries on the left side
    , G((m1 -> !c0) & (m2 -> !(c0 | c1)))

    // position of the boat changes each time
    , C(b)

    //if the boat is on the right side:
    , b -> and(

      // number of missionaries on the right side cannot increase
      m0 -> !N(m1 | m2 | m3)
      , m1 -> !N(m2 | m3)
      , m2 -> !N(m3)

      // number of cannibals on the right side cannot increase
      , c0 -> !N(c1 | c2 | c3)
      , c1 -> !N(c2 | c3)
      , c2 -> !N(c3)
    )

    //if the boat is on the left side:
    , !b -> and(

      // number of missionaries on the right side cannot decrease
      m1 -> !N(m0)
      , m2 -> !N(m0 | m1)
      , m3 -> !N(m0 | m1 | m2)

      // number of missionaries on the right side cannot decrease
      , c1 -> !N(c0)
      , c2 -> !N(c0 | c1)
      , c3 -> !N(c0 | c1 | c2)
    )

    // each time number of missionaries or number od cannibals changes
    , missionariesAreMoving | cannibalsAreMoving

    // the rules below describes that at most two people can pass the river at the same time
    //if only missionaries are passing the river:
    , !cannibalsAreMoving -> and(

      // the difference in number of missionaries cannot be greater than 2:
      m0 -> N(m1 | m2)
      , m1 -> N(m0 | m2 | m3)
      , m2 -> N(m0 | m1 | m3)
    )

    //if only cannibals are passing the river:
    , !missionariesAreMoving -> and(

      // the difference in number of cannibals cannot be greater than 2:
      c0 -> N(c1 | c2)
      , c1 -> N(c0 | c2 | c3)
      , c2 -> N(c0 | c1 | c3)
    )

    //if one cannibals and one of missionaries are passing the river together:
    , (cannibalsAreMoving & missionariesAreMoving) -> and(

      // the difference in number of cannibals and missionaries must equal 2:
      (m0 & c0) -> N(m1 & c1)
      , (m0 & c1) -> N(m1 & c2)
      , (m0 & c2) -> N(m1 & c3)
      , (m1 & c0) -> N(m2 & c1)
      , (m1 & c1) -> N((m0 & c0) | (m2 & c2))
      , (m1 & c2) -> N((m0 & c1) | (m2 & c3))
      , (m1 & c3) -> N(m0 & c2)
      , (m2 & c0) -> N(m3 & c1)
      , (m2 & c1) -> N((m1 & c0) | (m3 & c2))
      , (m2 & c2) -> N((m1 & c1) | (m3 & c3))
      , (m2 & c3) -> N(m1 & c2)
      , (m3 & c1) -> N(m2 & c0)
      , (m3 & c2) -> N(m2 & c1)
      , (m3 & c3) -> N(m2 & c2)
    )
  )

  // all people are on the left side
  val from = m0 & c0
  // all people are on the right side
  val to = m3 & c3

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
      val es = x.asInstanceOf[And].es - b
      val vars = es.filterNot(_.isInstanceOf[Neg]).map(_.asInstanceOf[Var])
      val m = vars.find(_.name.startsWith("m")).get
      val c = vars.find(_.name.startsWith("c")).get

      val mc = (m, c) match {
        case (_, _) if (m, c) == (m0, c0) => ("M M M C C C", "")
        case (_, _) if (m, c) == (m0, c1) => ("M M M C C ", "C ")
        case (_, _) if (m, c) == (m0, c2) => ("M M M C", "C C ")
        case (_, _) if (m, c) == (m0, c3) => ("M M M C", "C C C")

        case (_, _) if (m, c) == (m1, c0) => ("M M C C C", "M")
        case (_, _) if (m, c) == (m1, c1) => ("M M C C", "M C" )
        case (_, _) if (m, c) == (m1, c2) => ("M M C", "M C C")
        case (_, _) if (m, c) == (m1, c3) => ("M M", "M C C C")

        case (_, _) if (m, c) == (m2, c0) => ("M C C C", "M M")
        case (_, _) if (m, c) == (m2, c1) => ("M C C ", "M M C")
        case (_, _) if (m, c) == (m2, c2) => ("M C", "M M C C ")
        case (_, _) if (m, c) == (m2, c3) => ("M", "M M C C C")

        case (_, _) if (m, c) == (m3, c0) => ("C C C", "M M M")
        case (_, _) if (m, c) == (m3, c1) => ("C C ", "M M M C")
        case (_, _) if (m, c) == (m3, c2) => ("C", "M M M C C ")
        case (_, _) if (m, c) == (m3, c3) => ("", "M M M C C C")

      }

      printf("%-11.30s --- %-20.30s%n", mc._1, mc._2)
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
