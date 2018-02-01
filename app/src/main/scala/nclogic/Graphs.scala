package nclogic

import nclogic.model.expr.Expr._
import nclogic.model.expr.{N, _}

object Graphs extends App {

  var a = Term("a")
  var b = Term("b")
  var c = Term("c")
  var d = Term("d")

  var nodeA = a & !b & !c & !d
  var nodeB = !a & b & !c & !d
  var nodeC = !a & !b & c & !d
  var nodeD = !a & !b & !c & d

  val graph = or(
    nodeA & N(nodeB | nodeC),
    nodeB & N(nodeD),
    nodeC & N(nodeD),
    nodeD & N(nodeA)
  )

  var g = LncInferenceEngine.getHistoryGraph(graph)
  //var str = QueryEngine.getAllPaths(g).map(_.toString).mkString("\n")
    //println(str)
  print(g.prettyString)

}

