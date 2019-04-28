package nclogic.sat

import scala.collection.mutable


class Graph[N, E] {

  private val nodes = mutable.Set.empty[N]
  private val edges = mutable.HashMap.empty[N, mutable.HashMap[N, E]]

  def hasNode(n: N): Boolean = nodes.contains(n)

  def findNode(predicate: N => Boolean): Option[N] = nodes.find(predicate)

  def filterNodes(predicate: N => Boolean): Set[N] = nodes.filter(predicate).toSet

  def existsNode(predicate: N => Boolean): Boolean = findNode(predicate).isDefined

  def addNode(n: N): Unit = {
    if (!nodes.contains(n)) {
      nodes += n
      edges.put(n, mutable.HashMap.empty[N, E])
    }
  }

  def addEdge(n1: N, n2: N, e: E): Unit = {
    addNode(n1)
    addNode(n2)
    edges(n1).put(n2, e)
  }

  def getEdge(n1: N, n2: N): Option[E] = {
    edges.get(n1).flatMap(_.get(n2))
  }

  def getSuccessors(n: N): List[(N, E)] = {
    edges.get(n).map(_.toList).getOrElse(Nil)
  }

  def print = {
    val idx = nodes.zipWithIndex.toMap

    for {
      (n1, map) <- edges
      (n2, e) <- map
    } {
      println(idx(n1), idx(n2), e)
    }
  }
}
