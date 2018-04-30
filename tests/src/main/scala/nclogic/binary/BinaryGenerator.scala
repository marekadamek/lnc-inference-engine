package nclogic.binary

import nclogic.model.{BinaryGraph, HistoryGraph}

import scala.util.Random

object BinaryGenerator {

  def getRandom[A](list: List[A]) = list(Random.nextInt(list.size))

  def generate(length: Int, graph: BinaryGraph): List[Boolean] = {
    val start = getRandom(graph.getAllNodes.toList)
    generate(length, graph, start)
  }

  def generate(length: Int, graph: BinaryGraph, start: List[Boolean]): List[Boolean] = {

    def gen(current: List[Boolean], acc: List[List[Boolean]], l: Int): List[List[Boolean]] = l match {
      case x if x >= length => acc.reverse
      case _ =>
        val s = getRandom(graph.getSuccessors(current).toList)
        val last = s.takeRight(graph.baseTermsSize)
        gen(s, last :: acc, l + last.length)
    }

    gen(start, List(start), start.length).flatten
  }

  def generateCycles(length: Int, graph: HistoryGraph, cycles: List[List[Boolean]], startNode: List[Boolean]): List[Boolean] = {
    def gen(acc: List[List[Boolean]], l: Int): List[List[Boolean]] = {
      if (l >= length) {
        (startNode :: acc).reverse
      } else {
        val c = getRandom(cycles)
        gen(c :: acc, l + c.length)
      }
    }

    gen(Nil, 0).flatten
  }
}
