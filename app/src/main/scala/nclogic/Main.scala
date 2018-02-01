package nclogic

import ctlstar._
import nclogic.model.expr.Expr._
import nclogic.model.expr.{N, Var, _}

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
    (red & !yellow) -> N(red & yellow & constraints),
    (red & yellow) -> N(green & constraints),
    green -> N(!red & yellow & constraints),
    (!red & yellow) -> N(red & !yellow & constraints),
    constraints
  )

  val x = Var("X")
  val jan = Term("jan")
  val marek = Term("marek")
  val test = and(
    Predicate("male", List(jan)),
    Predicate("male", List(marek)),
    Predicate("human", List(x)) `<-` Predicate("male", List(x))
  )

  println(QueryEngine.runQuery(CtlQuery(Exists, Finally(!red & yellow & green)), trafficLights))
 // println(LncInferenceEngine.query(red & N(yellow) & N(N(green)) & N(N(N(yellow))) & N(N(N(N(red)))), trafficLights))


  //println(LncInferenceEngine.query(Predicate("human", List(jan)), test))


}

