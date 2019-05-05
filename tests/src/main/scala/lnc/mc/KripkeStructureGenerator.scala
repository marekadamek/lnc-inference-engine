package lnc.mc

import lnc.kripke.{KripkeStructure, KripkeStructureNode}
import lnc.expr.{Expr, Not, True, Var}

import scala.collection.mutable

object KripkeStructureGenerator {
  private val rnd = new scala.util.Random

  private def randomNodeId(limit: Int) = 1 + rnd.nextInt(limit)

  private def generateRandomPrepositions(vars: List[Var]): Set[Expr] = {
    val vCount = rnd.nextInt(vars.size + 1)

    if (vCount == 0) {
      Set(True)
    } else {
      val vNegs = rnd.nextInt(vCount + 1)

      val preps = rnd.shuffle(vars).take(vCount)
      val negs = preps.take(vNegs).map(Not)

     (preps.drop(vNegs) ++ negs).map(_.asInstanceOf[Expr]).toSet
    }
  }

  def generateRandomStructure(nodesCount: Int, initialNodes: Int, edgesCount: Int, prepositions: Int): KripkeStructure = {
    require(initialNodes <= nodesCount)
    require(edgesCount >= nodesCount && edgesCount <= nodesCount * nodesCount)

    val initials = rnd.shuffle(1 to nodesCount).take(initialNodes).toSet

    val vars = (for (i <- 1 to prepositions) yield Var(s"p$i")).toList

    var structure = new KripkeStructure
    val edges = mutable.Set.empty[(Int, Int)]

    for (id <- 1 to nodesCount) {
      val preps = generateRandomPrepositions(vars)
      val node = KripkeStructureNode(id, preps, initials.contains(id))
      structure = structure.addNode(node)
      edges.add((id, randomNodeId(nodesCount)))
    }

    while (edges.size < edgesCount) {
      val e = (randomNodeId(nodesCount), randomNodeId(nodesCount))

      if (!edges.contains(e._1, e._2)) {
        edges.add(e._1, e._2)
      }
    }

    edges.foreach { case (from, to) =>
      structure = structure.addEdge(from, to)
    }

    structure
  }

}
