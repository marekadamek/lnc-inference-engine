package nclogic.mcheck

import nclogic.model.HistoryGraph
import nclogic.model.expr.{Neg, TemporalExpr, Var}

object MCheckConverter {

  def convert1(graph: HistoryGraph): String = {
    val nodes = graph.getAllNodes
    val nodesMap = nodes.zipWithIndex.map(p => p._1 -> ("S" + p._2, p._1.getTerms.filterNot(_.isInstanceOf[TemporalExpr]).filterNot(_.isInstanceOf[Neg]))).toMap

    val nodesSection = nodesMap.values.map(p => {
      val label = p._2.map(_.toString).mkString(",")
      "" + p._1 + " [label = \"" + label + "\"]"
    }).mkString("\n")

    val edgesSection = nodes.flatMap(n => {
      val sucessors = graph.getSuccessors(n)
      sucessors.map(s => {
        val from = nodesMap(n)._1
        val to = nodesMap(s)._1
        from + " -> " + to
      })
    }).mkString("\n")

    "digraph G {\n" + edgesSection + "\n\n" + nodesSection + "\n}"
  }

  /** *
    * {
    * >	0 1 2 p q
    *   1 0 2 q r
    * >	2 2   r
    * }
    */
  def convert2(graph: HistoryGraph): String = {
    val nodes = graph.getAllNodes
    val nodesMap = nodes.zipWithIndex.map(p => p._1 -> (p._2, p._1.getTerms.filterNot(_.isInstanceOf[TemporalExpr]).filterNot(_.isInstanceOf[Neg]))).toMap

    val lines = nodes.map(n => {
      val nInfo = nodesMap(n)
      val sucessorsIds = graph.getSuccessors(n).map(s => nodesMap(s)._1).mkString(" ")
      val valuation = if (nInfo._2.isEmpty) "empty" else nInfo._2.mkString(" ")
      ">\t" + nInfo._1 + "\t" + sucessorsIds + "\t" + valuation
    })

    "{\n" + lines.mkString("\n") + "\n}"
  }

}
