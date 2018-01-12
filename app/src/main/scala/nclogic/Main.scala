package nclogic

import nclogic.model.expr.Expr._
import nclogic.model.expr.{N, Var, _}
import nclogic.model.Formula._
import time._

object Main extends App {

  val red: Block = Block("r", () => println("red"))
  val green: Block = Block("g", () => println("green"))
  val yellow: Block = Block("y", () => println("yellow"))

  val constraints = and(
    !(red & green & yellow),
    !(red & green),
    !(green & yellow),
    red | green | yellow
  )

  val trafficLights = and(
    (red & !yellow & !green) -> N(red & yellow & !green & constraints),
    (red & yellow & !green) -> N(!red & !yellow & green & constraints),
    (!red & !yellow & green) -> N(!red & yellow & !green & constraints),
    (!red & yellow & !green) -> N(red & !yellow & !green & constraints),
    constraints
  )

  LncInferenceEngine.evaluate(trafficLights, red & !yellow & !green)
  //e.eval(Map((a, True)))

  //  val measurement = measureTime {
  //    //println(LncInferenceEngine.queryPaths(red & N(yellow) & N(N(green)) & N(N(N(yellow))) & N(N(N(N(red)))), trafficLights))
  //     LncInferenceEngine.query(red & yellow & green, trafficLights)
  //  }
  //
  //  println(measurement.time)
  //  println(measurement.result)

  //LncInferenceEngine.evaluate(trafficLights, red & !yellow & !green)
  //  println(LncInferenceEngine.isTautology(a | !a))
  //  println(x.matches(y))
  //  println(y.matches(x))
  //println(Expr.or(x, y))
  //println(e exists until(a, c))
  //val d =  N(C(a) & b & N(c)).simplify
  //println(LncInferenceEngine.getHistoryGraph(e))

  //e.paths foreach println
  //println(LncInferenceEngine.getHistoryGraph(e).getSuccessors(Set[Expr](a, !b)))

}

