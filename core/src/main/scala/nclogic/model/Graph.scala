package nclogic.model

import nclogic.LncInferenceEngine
import nclogic.model.DnfConverter.{AndClause, DNF}
import nclogic.model.Types._

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
  private val dnf = DnfConverter.convert(formula)
  lazy val nodes = LncInferenceEngine.getPositiveValuations(formula)

  def getSuccessors(and: AndClause) = {
    val x = dnf.filterNot(c => c.exists(e1 => and.exists(e2 => e1 == Neg(e2).simplify)))
    val nextStates = dnf
      .filterNot(c => c.exists(e1 => and.exists(e2 => e1 == Neg(e2).simplify)))
      .map(nextStep)

    nodes.filter(n => nextStates.exists(ns => (ns & n) == ns) && n != and)
  }

  private def nextStep(state: AndClause): AndClause = {
    val ns = state.filter(_.isInstanceOf[N]).map(_.asInstanceOf[N])
    (state--ns).filterNot(e => ns.exists(n => n == N(e) || n == N(Neg(e).simplify))) ++ ns.map(_.e)
  }

}