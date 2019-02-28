//package nclogic.problems
//
//import nclogic.model.HistoryGraph
//import nclogic.model.converters.CnfConverter
//import nclogic.model.expr.Expr.or
//import nclogic.model.expr.{C, _}
//import nclogic.sat.Sat
//
//object test {
//
//  val a = Var("a")
//  val b = Var("b")
//  val c = Var("c")
//
//  val problem: Expr = or(
//    a & !b & !c & N((a & b & !c) | (a & !b & !c)),
//    a & b & !c & N(a & b & c)
//  )
//
//  def main(args: Array[String]): Unit = {
//    val cnfMeasure = time.measureTime {
//      CnfConverter.convert(problem)
//    }
//
//    val satMeasure = time.measureTime {
//      Sat.solve(cnfMeasure.result)
//    }
//
//    println(satMeasure.result)
//    println()
//
//    val graphMeasure = time.measureTime {
//      HistoryGraph(satMeasure.result)
//    }
//
//    graphMeasure.result.getAllNodes.foreach(e => {
//      println(e)
//      println(graphMeasure.result.getSuccessors(e))
//      println()
//    })
//  }
//
//}
