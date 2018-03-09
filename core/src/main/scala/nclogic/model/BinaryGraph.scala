package nclogic.model

import nclogic.model.expr._


case class BinaryGraph(private val historyGraph: HistoryGraph) {

  private def termToBool(e: Expr): Boolean = e match {
    case Term(_) => true
    case Neg(Term(_)) => false
    case Next(x) => termToBool(x)
    case _ => throw new IllegalArgumentException("Illegal")
  }

  private def exprToBool(e: Expr): List[Boolean] = e.getTerms.map(termToBool)

  val edges: Map[List[Boolean], Set[List[Boolean]]] = historyGraph.getAllNodes.map(node => {
    val successors = historyGraph.getSuccessors(node)
    (exprToBool(node), successors.map(exprToBool))
  }).toMap

  val baseTermsSize: Int = historyGraph.baseTerms.size
  val level: Int = historyGraph.level

  def getSuccessors(node: List[Boolean]): Set[List[Boolean]] = edges(node)

  def contains(node: List[Boolean]): Boolean = edges.keySet.contains(node)

  def getAllNodes = edges.keySet
}