package lnc.model

import lnc.expr._


//case class BinaryGraph(private val historyGraph: HistoryGraph) {
//
//  private def termToBool(e: Expr): Boolean = e match {
//    case Var(_) => true
//    case Not(Var(_)) => false
//    case Next(x) => termToBool(x)
//    case _ => throw new IllegalArgumentException("Illegal")
//  }
//
//  private def exprToBool(e: Expr): List[Boolean] = e.getTerms.toList.map(termToBool)
//
//  val edges: Map[List[Boolean], Set[List[Boolean]]] = historyGraph.getAllNodes.map(node => {
//    val successors = historyGraph.getSuccessors(node)
//    (exprToBool(node), successors.map(exprToBool))
//  }).toMap
//
//  val baseTermsSize: Int = historyGraph.baseTerms.size
//  val level: Int = historyGraph.level
//
//  def getSuccessors(node: List[Boolean]): Set[List[Boolean]] = edges(node)
//
//  def contains(node: List[Boolean]): Boolean = edges.keySet.contains(node)
//
//  def getAllNodes: Set[List[Boolean]] = edges.keySet
//}