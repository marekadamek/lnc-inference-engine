package nclogic.model

import nclogic.model.DnfConverter.AndClause
import nclogic.model.Types._
import nclogic.sat.Sat

trait Graph[T] {
  def nodes: Set[T]

  def getSuccessors(node: T): Set[_ <: T]

  def findPath(from: T, to: T): List[AndClause]

}


case class HistoryGraph(protected val formula: Expr) extends Graph[AndClause] {
  val cnf = CnfConverter.convert(formula)
  private val valuations = Sat.solve(CnfConverter.convert(formula))

  val pairs = for {
    i <- List(0)
    v <- valuations
    from = getElementsFromLevel(v, i)
    to = getElementsFromLevel(v, i + 1)
  } yield {
      from -> to
    }

  lazy val nodes = pairs.map(_._1).toSet

  def isN(e: Expr) = e match {
    case N(_) => true;
    case Neg(N(_)) => true
    case _ => false
  }

  def getElementsFromLevel(valuation: AndClause, level: Int): AndClause = level match {
    case 0 => valuation filterNot isN
    case _ =>
      val nextValuations = valuation.filter(isN).map {
        case N(e) => e;
        case Neg(N(e)) => Neg(e)
      }
      getElementsFromLevel(nextValuations, level - 1)
  }

  def getSuccessors(clause: AndClause) = {
    pairs
      .filter(p => p._1 forall clause.contains)
      .map(_._2)
      .map(to => clause -- to.map(Neg).map(_.simplify) ++ to)
      .toSet
  }

  override def findPath(from: AndClause, to: AndClause) = {

    def loop(toDo: List[List[AndClause]], done: Set[AndClause]): List[AndClause] = toDo match {
      case Nil => Nil
      case path :: paths =>
        val current = path.head

        if (to forall current.contains) path
        else {
          val succs = getSuccessors(current).diff(done)
          val newPaths = succs.toList map { _ :: path }
          loop(paths ++ newPaths, done + current)
        }
    }

    loop(List(List(from)), Set.empty).reverse
  }

  override def toString: String = {
    pairs map { case (k, v) => k + " -> " + v } mkString "\n"
  }

}