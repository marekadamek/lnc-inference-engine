package nclogic.problems

import kripke.LNCToKripkeStructureConverter
import nclogic.model.PrefixFormulaConverter
import nclogic.model.expr.Expr._
import nclogic.model.expr.{C, _}
import nclogic.sat.SatSolvers

/**
  *
  * Japanese IQ Test - River Crossing
  *
  * The goal of the game is to transport all people (policeman, thief, mother, father, two boys and two girls)
  * from one side of the river to the other. Below are the rules of the game:
  *
  * - the raft can take only two persons at the same time,
  * - the father cannot stay with any of the daughters without the presence of the mother,
  * - the mother cannot stay with any of the sons without the presence of the father,
  * - the thief (with striped shirt) cannot stay with any family member without the presence of the policeman,
  * - the raft can only be operated by the policeman, the father and the mother.
  */
object JapaneseIQTestRiverCrossing {

  /**
    *
    * Variable definition:
    * p - policeman
    * t - thief
    * m - mother
    * f - father
    * b1 - first boy
    * b2 - second boy
    * g1 - first girl
    * g2 - second girl
    * b - boat
    *
    * If variable is false it means that the related being is on the left side,
    * if it's true the the being is on the right side
    */
  val p = Var("p")
  val t = Var("t")
  val m = Var("m")
  val f = Var("f")
  val b1 = Var("b1")
  val b2 = Var("b2")
  val g1 = Var("g1")
  val g2 = Var("g2")
  val b = Var("b")

  val all = Set(p, t, b1, b2, g1, g2, m, f)
  val navigators = Set(p, f, m)

  /**
    * The code below generates a conjunction of following rules:
    *
    * (C(x) & C(y)) -> !(C(z1) | C(z2 | ... | C(zn))
    *
    * where
    * x is one of (p, m, f)
    * y is one of all variables except x
    * z1..zn is are all variables except x and y
    */
  val atMostTwoPeopleAtOnce = {
    val rules = navigators.flatMap(nav => {
      val rest = all - nav
      rest.map(passenger => {
        val toStay = rest - passenger
        (C(nav) & C(passenger)) -> !Or.formSet(toStay.map(C))
      })
    })
    And.fromSet(rules)
  }

  def goesRight(x: Expr) = !x & N(x)

  def goesLeft(x: Expr) = x & N(!x)


  val problem: Expr = and(
    // always thief has to be on the same side as policeman
    // or all other people have to be on the opposite side to the thief
    (t <-> p) | And.fromSet(Set(b1, b2, g1, g2, m, f).map(_ <-> !t))

    // always mather needs to be on the same side as father
    // or boys have to be on the opposite side to the mother
    , (m <-> f) | ((b1 <-> !m) & (b2 <-> !m))

    // always mather needs to be on the same side as father
    // or girls have to be on the opposite side to the father
    , (m <-> f) | ((g1 <-> !f) & (g2 <-> !f))

    // if the boat is on the right side then nobody goes right
    , b -> !Or.formSet(navigators.map(goesRight))

    // if the boat is on the left side then nobody goes left
    , !b -> !Or.formSet(navigators.map(goesLeft))

    // position of the boat always changes
    , C(b)

    // each time policeman, mother or father changes side
    , C(p) | C(m) | C(f)

    // children cannot pass the river alone
    , (C(g1) | C(g2) | C(b1) | C(b2)) -> (C(p) | C(m) | C(f))

    // thief can only pass the river with policeman
    , C(t) -> C(p)

    // at most two people can pass the river at the same time
    // see the full code above
    , atMostTwoPeopleAtOnce
  )

  // everyone is on the left side and boat is on the left side
  val from = Set[Expr](!b, !p, !t, !b1, !b2, !g1, !g2, !m, !f)

  // everyone is on the right side
  val to = Set[Expr](b, p, t, b1, b2, g1, g2, m, f)

  def main(args: Array[String]): Unit = {

    val (kripke, graphMeasure) = time.measureTime {
      LNCToKripkeStructureConverter.convert(problem, SatSolvers.miniSat)
    }

    val (path, pathMeasure) = time.measureTime {
      kripke.findPathBFS(from, to)
    }

    path.get.map(_.terms).foreach(x => {
      x.filter(_.isInstanceOf[Not]).map(_.asInstanceOf[Not].e).foreach(x => print(x + " "))
      print("  -----   ")
      x.filterNot(_.isInstanceOf[Not]).foreach(x => print(x + " "))
      println()
    })

    println()
    println("Execution time (s):")
    println("Building graph: " + graphMeasure.seconds)
    println("Finding solution path: " + pathMeasure.seconds)
    println("TOTAL: " + List(graphMeasure, pathMeasure).map(_.seconds).sum)
  }
}
