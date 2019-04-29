package kripke

import nclogic.model.expr.Expr

class KripkeStructure(val nodes: Map[Int, KripkeStructureNode] = Map.empty[Int, KripkeStructureNode],
                      val edges: Map[Int, Set[Int]] = Map.empty[Int, Set[Int]]) {

  def nodeExists(id: Int): Boolean = nodes.contains(id)

  def getInitialNodes: Set[KripkeStructureNode] = nodes.values.filter(_.initial).toSet

  def getSuccessors(node: Int): Set[KripkeStructureNode] = {
    edges(node).filter(nodes.contains).map(nodes)
  }

  def addNode(n: KripkeStructureNode): KripkeStructure = {
    if (nodeExists(n.id)) {
      throw new Exception(s"Node with id ${n.id} already exists in Kripke structure")
    }

    val newNodes = nodes + ((n.id, n))
    val newEdges = edges + ((n.id, Set.empty[Int]))
    new KripkeStructure(newNodes, newEdges)
  }

  def addEdge(from: Int, to: Int): KripkeStructure = {
    (for {
      _ <- nodes.get(from)
      _ <- nodes.get(to)
    } yield {
      val set = edges(from)
      val newEdges = edges.updated(from, set + to)
      new KripkeStructure(nodes, newEdges)
    }) getOrElse {
      throw new Exception(s"At least one node with id $from or $to does not belong in Kripke structure")
    }
  }

  def removeNode(id: Int): KripkeStructure = {
    val newNodes = nodes - id
    val newEdges = (edges - id).mapValues(_ - id)

    new KripkeStructure(newNodes, newEdges)
  }

  def nodesCount: Int = nodes.size

  def initialNodesCount: Int = nodes.values.count(_.initial)

  def edgesCount: Int = edges.map(_._2.size).sum

  def getNodes: Set[KripkeStructureNode] = nodes.values.toSet

  private def bfs(nodes: List[List[Int]], visited: Set[Int], goal: Set[Int]): Option[List[Int]] = nodes match {
    case Nil => None
    case path :: tail =>
      if (goal.contains(path.head)) Some(path.reverse)
      else {
        if (visited.contains(path.head)) {
          bfs(tail, visited, goal)
        } else {
          val newPaths = edges(path.head).map(_ :: path).toList
          bfs(tail ++ newPaths, visited + path.head, goal)
        }
      }
  }

  def findPathBFS(from: Set[Expr], to: Set[Expr]): Option[List[KripkeStructureNode]] = {
    val startNodes = nodes.values.filter(n => from.forall(n.terms.contains)).map(_.id).toList
    val goalNodes = nodes.values.filter(n => to.forall(n.terms.contains)).map(_.id).toSet

    bfs(startNodes.map(List(_)), Set.empty, goalNodes).map(_.map(nodes.apply))
  }
}
