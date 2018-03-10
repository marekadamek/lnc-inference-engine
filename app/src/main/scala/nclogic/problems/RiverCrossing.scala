package nclogic.problems

import nclogic.model.HistoryGraph
import nclogic.model.converters.CnfConverter
import nclogic.model.expr.Expr._
import nclogic.model.expr.{C, _}
import nclogic.sat.Sat

object RiverCrossing {

  val policeman = Var("policeman")
  val thief = Var("thief")
  val mother = Var("mother")
  val father = Var("father")
  val boy1 = Var("boy1")
  val boy2 = Var("boy2")
  val girl1 = Var("girl1")
  val girl2 = Var("girl2")
  val boat = Var("boat")

  val goRight = (x: Var) => !x & N(x)
  val goLeft = (x: Var) => x & N(!x)

  val problem: Expr = and(
    G((thief <-> policeman) | and(boy1 <-> !thief, boy2 <-> !thief, girl1 <-> !thief, girl2 <-> !thief, mother <-> !thief, father <-> !thief))
    , G((mother <-> father) | ((boy1 <-> !mother) & (boy2 <-> !mother)))
    , G((mother <-> father) | ((girl1 <-> !father) & (girl2 <-> !father)))
    , boat -> !(goRight(policeman) | goRight(thief) | goRight(father) | goRight(mother) | goRight(boy1) | goRight(boy2) | goRight(girl1) | goRight(girl2))
    , !boat -> !(goLeft(policeman) | goLeft(thief) | goLeft(father) | goLeft(mother) | goLeft(boy1) | goLeft(boy2) | goLeft(girl1) | goLeft(girl2))

    , C(boat)
    , C(policeman) | C(mother) | C(father)
    , (C(girl1) | C(girl2) | C(boy1) | C(boy2)) -> (C(policeman) | C(mother) | C(father))
    , C(thief) -> C(policeman)

    /////////////////////////////////////////////////
    , (C(policeman) & C(thief)) -> (!C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(mother) & !C(father))
    , (C(policeman) & C(boy1)) -> (!C(thief) & !C(boy2) & !C(girl1) & !C(girl2) & !C(mother) & !C(father))
    , (C(policeman) & C(boy2)) -> (!C(thief) & !C(boy1) & !C(girl1) & !C(girl2) & !C(mother) & !C(father))
    , (C(policeman) & C(girl1)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl2) & !C(mother) & !C(father))
    , (C(policeman) & C(girl2)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(mother) & !C(father))
    , (C(policeman) & C(mother)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(father))
    , (C(policeman) & C(father)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(mother))

    , (C(mother) & C(boy1)) -> (!C(thief) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman) & !C(father))
    , (C(mother) & C(boy2)) -> (!C(thief) & !C(boy1) & !C(girl1) & !C(girl2) & !C(policeman) & !C(father))
    , (C(mother) & C(girl1)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl2) & !C(policeman) & !C(father))
    , (C(mother) & C(girl2)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(policeman) & !C(father))
    , (C(mother) & C(father)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman))

    , (C(father) & C(boy1)) -> (!C(thief) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(boy2)) -> (!C(thief) & !C(boy1) & !C(girl1) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(girl1)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(girl2)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(policeman) & !C(mother))
  )

  val start = !boat & !policeman & !thief & !boy1 & !boy2 & !girl1 & !girl2 & !mother & !father
  val end = policeman & thief & boy1 & boy2 & girl1 & girl2 & mother & father

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
      graphMeasure.result.findPath(start, end)
    }

    pathMeasure.result foreach { x => {
      val es = x.asInstanceOf[And].es
      es.filter(_.isInstanceOf[Neg]).foreach(x => print(x + " "))
      print("  -----   ")
      es.filterNot(_.isInstanceOf[Neg]).foreach(x => print(x + " "))
      println()
    }
    }
    println(pathMeasure.result.size)

    println()
    println("Time:")
    println("CNF: " + cnfMeasure.microTime / 1000)
    println("SAT: " + satMeasure.microTime / 1000)
    println("Graph: " + graphMeasure.microTime / 1000)
    println("Path: " + pathMeasure.microTime / 1000)
    println("TOTAL: " + List(cnfMeasure, satMeasure, graphMeasure, pathMeasure).map(_.microTime).sum / 1000)

  }

}
