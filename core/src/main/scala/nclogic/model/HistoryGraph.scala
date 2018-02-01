package nclogic.model

import nclogic.graph.{Edge, Graph}
import nclogic.model.converters.CnfConverter
import nclogic.model.expr._
import nclogic.sat.Sat


case class HistoryGraph(protected val formula: Expr) {

  val (graph, startNodes) = {
    val cnf = CnfConverter.convert(formula)
    val valuations = Sat.solve(cnf)

    var graph = Graph.empty[Expr]

    val pairs = {
      def processClause(clause: Expr): List[Edge[Expr]] = {
        val nextClause = getNext(clause)
        val pair = Edge[Expr](clause, nextClause)

        if (nextClause == clause) List(pair)
        else pair :: processClause(nextClause)
      }

      def loop(expr: Expr): List[Edge[Expr]] = expr match {
        case Or(es) => es flatMap loop
        case clause =>
          val node = clause.simplify
          graph = graph.addNode(node)
          processClause(clause)
      }

      loop(valuations)
    }

    pairs.foreach(p => graph = graph.addEdge(p))
    (graph, valuations.asInstanceOf[Or].es)
  }

  def getNext(clause: Expr): Expr = {
    val (temporal, nonTemporal) = clause.getTerms.partition(_.isInstanceOf[TemporalExpr])
    val next = temporal.map(_.asInstanceOf[TemporalExpr]).map(_.e)
    val base = nonTemporal
      .filterNot(e => next.contains(Neg(e).simplify))

    And(base ++ next).simplify
  }

  def getSuccessors(clause: Expr): Set[Expr] = {
    graph.edges
      .filter(edge => Expr.and(edge.from, clause).simplify != False)
      .map(_.to)
      .map(to => {
        And(clause.getTerms.filterNot(to.getTerms.map(Neg).map(_.simplify).contains) ++ to.getTerms).simplify
      })
  }

  def findPath(from: Expr, to: Expr) = {
    def loop(toDo: List[List[Expr]], done: Set[Expr]): List[Expr] = toDo match {
      case Nil => Nil
      case path :: paths =>
        val current = path.head

        if (Expr.and(to, current).simplify != False) path
        else {
          val succs = getSuccessors(current).diff(done)
          val newPaths = succs.toList map {
            _ :: path
          }
          loop(paths ++ newPaths, done + current)
        }
    }

    loop(List(List(from)), Set.empty).reverse
  }

  override def toString: String = {
    graph.edges map (e => e.from + " -> " + e.to) mkString "\n"
  }

  def prettyString: String = {
    def prettyNode = (n: Expr) => And(n.getTerms.filterNot(_.isInstanceOf[Next])).simplify

    graph.edges map (e => prettyNode(e.from) + " -> " + prettyNode(e.to)) mkString "\n"
  }

}