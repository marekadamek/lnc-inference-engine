package lnc.problems

import lnc.expr.Expr._
import lnc.expr._
import lnc.kripke.{KripkeStructureNode, LNCToKripkeStructureConverter}
import lnc.sat.SatSolvers

case class TrafficLights(id: Int) {
  val p: Var = Var("p" + id)
  val q: Var = Var("q" + id)

  val go = p & q
  val ready = p & !q
  val stop = !p & q
  val warning = !p & !q

  def exactlyOneTrue(es: Set[Expr]): Expr = {
    val ands = es.map(v => {
      val rest = es - v
      Expr.and(rest.map(!_) + v)
    })
    Expr.or(ands)
  }

  lazy val expr = and(
    //exactlyOneTrue(Set(go, ready, stop, warning)),
    go -> N(warning),
    warning -> N(stop),
    stop -> N(ready),
    ready -> N(go)
  )
}

object TrafficLights_ extends App {
  val tl1 = TrafficLights(1).expr

  val kripke = LNCToKripkeStructureConverter.convert(tl1, SatSolvers.tableAux)

  val start = kripke.nodes.values.head
  kripke.prettyPrint()

}


object TrafficLightsSimulation extends App {
  val tl1 = TrafficLights(1)
  val tl2 = TrafficLights(2)

  val crossRoads = and(
    tl1.expr,
    tl2.expr,
    !(tl1.go & (tl2.go | tl2.warning)),
    !(tl2.go & (tl1.go | tl1.warning))
  )

  val x = crossRoads.simplify
  val kripke = LNCToKripkeStructureConverter.convert(crossRoads, SatSolvers.tableAux)

  val start = kripke.nodes.values.head

  def simulate(path: List[KripkeStructureNode]): List[KripkeStructureNode] = {
    val e = and(path.head.terms)
    println("State: " + path.head.id)

    List(
      (tl1.go, "TF 1 -> go"),
      (tl1.ready, "TF 1 -> ready"),
      (tl1.stop, "TF 1 -> stop"),
      (tl1.warning, "TF 1 -> warning"),

      (tl2.go, "TF 2 -> go"),
      (tl2.ready, "TF 2 -> ready"),
      (tl2.stop, "TF 2 -> stop"),
      (tl2.warning, "TF 2 -> warning")
    )
      .filter(x => (x._1 & e).simplify != False)
      .foreach(x => println(x._2))



    if (!path.tail.contains(path.head)) {
      val next = kripke.getSuccessors(path.head.id).head
      simulate(next :: path)
    } else {
      path.reverse
    }
  }

  val path = simulate(List(start))

  path.foreach(m => println(m.terms))

  kripke.prettyPrint()
}
