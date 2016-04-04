package nclogic.model

import nclogic.model.DnfConverter.AndClause
import nclogic.model.Types._
import nclogic.sat.Sat

trait Graph[T] {
  def nodes: Set[T]

  def getSuccessors(node: T): Set[_ <: T]

}


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

  def getSuccessors(clause: AndClause) = {
    pairs
      .filter(p => p._1 forall clause.contains)
      .map(_._2)
      .map(to => clause -- to.map(Neg).map(_.simplify) ++ to)
      .toSet

      .filter(to => nodes.exists(_ forall to.contains))
  }

  override def toString(): String = {
    pairs map { case (k, v) => k + " -> " + v } mkString "\n"
  }

}