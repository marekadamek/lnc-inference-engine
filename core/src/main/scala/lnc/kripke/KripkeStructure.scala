package lnc.kripke

import lnc.expr.{Not, Var}

/**
  * Represents Kripke strukture
  *
  * @param nodes set of nodes kept as map with is as key
  * @param edges set of edges
  */
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

  def prettyPrint(): Unit = {
      def nodesStr(ts: KripkeStructureNode): String = {
        if (ts.terms.isEmpty) {
          "T"
        } else {
          ts.terms
            .filter {
              case Var(_) | Not(Var(_)) => true
              case _ => false
            }
            .toList.sortBy(_.toString).mkString(" ")
        }
      }

      for {
        i <- nodes.keys.toList.sorted
      } {
        println(i + " " + nodesStr(nodes(i)))
      }

      for {
        i <- nodes.keys.toList.sorted
        j <- edges(i).toList.sorted
      } {
        println(nodes(i).id + "  -->  " + nodes(j).id)
      }
    }
}


