package nclogic.binary

import nclogic.model.BinaryGraph
import nclogic.java.model.expr._

object BinaryValidator {

  def toBool(e: Expr): Boolean = e match {
    case Var(_) => true
    case Neg(Var(_)) => false
    case Next(x) => toBool(x)
    case _ => throw new IllegalArgumentException("Illegal")
  }

  def eq(l1: List[Boolean], l2: List[Boolean]) = l1.lengthCompare(l2.length) == 0 && l1.zip(l2).forall(p => p._1 == p._2)

  def validate(list: List[Boolean], graph: BinaryGraph): Boolean = {
    val n = graph.baseTermsSize
    val d = graph.level


    val start = list.take(n * d)
    validate(list, graph, start)
  }

  def validate(list: List[Boolean], graph: BinaryGraph, start: List[Boolean]): Boolean = {
    val n = graph.baseTermsSize
    val d = graph.level

    def loop(list: List[Boolean], node: List[Boolean]): Boolean = list match {
      case Nil => true
      case _ =>
        val sublist = list.take(n)
        graph.getSuccessors(node).find(s => eq(s.takeRight(n), sublist)) match {
          case None => false
          case Some(s) => loop(list.drop(n), s)
        }
    }

    if (graph.contains(start)) {
      loop(list.drop(n * d), start)
    }

    else false
  }

  def validateCycles(list: List[Boolean], cycles: List[List[Boolean]], startNode: List[Boolean]): Boolean = {
    val cyclesMap = cycles.map(c => (c, List(c,startNode).flatten))

    def loop(list: List[Boolean]): Boolean = list match {
      case Nil => true
      case _ =>
        cyclesMap.find(c => eq(list.take(c._2.length), c._2)) match {
          case None => eq(list, startNode)
          case Some(c) => loop(list.drop(c._1.length))
        }
    }

    loop(list)
  }
}
