package nclogic.model

import nclogic.model.DnfConverter.AndClause
import nclogic.model.Types._
import nclogic.sat.Sat

trait Graph[T] extends Traversable[T] {
  def nodes: Set[T]

  def getSuccessors(node: T): Set[_ <: T]

  override def foreach[U](f: (T) => U): Unit = {
    def loop(todo: List[T], traversed: List[T]): Unit = todo match {
      case Nil => ()
      case n :: ns if traversed contains n => loop(ns, traversed)
      case n :: ns =>
        f(n)
        loop(ns ++ getSuccessors(n), n :: traversed)
    }

    loop(nodes.toList, Nil)
  }

}


//class AdjacencyListGraph[T](protected val successorsMap: Map[T, Set[_ <: T]]) extends Graph[T] {
//  lazy val nodes = successo rsMap.keySet
//
//  override def getSuccessors(node: T) = successorsMap.getOrElse(node, Set.empty)
//
//  override def addNode(node: T, edges: Set[T]) = {
//    val newEntries = (edges + node).diff(nodes).map(n => n -> Set.empty)
//    var newSuccessorsMap = successorsMap ++ newEntries
//    newSuccessorsMap = newSuccessorsMap.updated(node, newSuccessorsMap(node) ++ edges)
//    new AdjacencyListGraph[T](newSuccessorsMap)
//  }
//
//  override def toString() = {
//    (for {
//      (k, v) <- successorsMap
//    } yield {
//        val successors = if (v.isEmpty) v else v mkString " | "
//        s"$k => $successors"
//      }) mkString "\n"
//
//  }
//}

case class HistoryGraph(protected val formula: Expr) extends Graph[AndClause] {
  private val valuations = Sat.solve(CnfConverter.convert(formula))
  val pairs = for {
    i <- List(0)
    v <- valuations
  } yield {
      val from = getElementsFromLevel(v, i)
      val to = getElementsFromLevel(v, i + 1)
      from -> to
    }

  lazy val nodes = pairs.map(_._1).toSet

  def getElementsFromLevel(valuation: AndClause, level: Int): AndClause = level match {
    case 0 => valuation.filterNot(_.isInstanceOf[N])
    case _ => getElementsFromLevel(valuation.filter(_.isInstanceOf[N]).map(_.asInstanceOf[N].e), level - 1)
  }

  def getSuccessors(node: AndClause) = pairs.filter(_._1 == node).map(_._2).toSet

}