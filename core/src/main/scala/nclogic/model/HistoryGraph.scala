package nclogic.model

import nclogic.graph.{Edge, Graph}
import nclogic.model.converters.CnfConverter
import nclogic.model.expr._
import nclogic.sat.Sat
import nclogic.tree.Tree


case class HistoryGraph(protected val formula: Expr) {

  val graph = {
    val cnf = CnfConverter.convert(formula)
    val valuations = Sat.solve(cnf)

    val pairs = {
      def processClause(clause: Expr): List[Edge[Expr]] = {
        val nextClause = getNext(clause)
        val shouldStop = nextClause == clause || nextClause == False
        if (shouldStop) List.empty
        else {
          val pair = Edge[Expr](stripTemporal(clause), stripTemporal(nextClause))
          pair :: processClause(nextClause)
        }
      }

      def loop(expr: Expr): List[Edge[Expr]] = expr match {
        case Or(es) => (es flatMap loop).toList
        case clause =>
          val firstPair = Edge[Expr](True, stripTemporal(clause))
          firstPair :: processClause(clause)

      }

      loop(valuations)
    }

    pairs.foldLeft(Graph.empty[Expr]) { (g, e) => g.addEdge(e) }
  }

  private def stripTemporal(v: Expr) = And(v.getTerms.filterNot(_.isInstanceOf[TemporalExpr]))

  def getNext(clause: Expr):Expr = {
    val (temporal, nonTemporal) = clause.getTerms.partition(_.isInstanceOf[TemporalExpr])
    val next = temporal.map(_.asInstanceOf[TemporalExpr]).map(_.e).toList
    val base = nonTemporal
      .filterNot(e => next.contains(Neg(e).simplify))

    And(base ++ next).simplify
  }

  def getSuccessors(clause: Expr) = {
    graph.edges
      .filter(edge => Expr.and(edge.from, clause).simplify != False)
      .map(_.to)
      .map(to => {
        And(clause.getTerms -- to.getTerms.map(Neg).map(_.simplify) ++ to.getTerms)
      })
  }

  def findPath(from: Expr, to: Expr) = {
  List.empty
//    def loop(toDo: List[List[AndClause]], done: Set[AndClause]): List[AndClause] = toDo match {
//      case Nil => Nil
//      case path :: paths =>
//        val current = path.head
//
//        if (to forall current.contains) path
//        else {
//          val succs = getSuccessors(current).diff(done)
//          val newPaths = succs.toList map {
//            _ :: path
//          }
//          loop(paths ++ newPaths, done + current)
//        }
//    }
//
//    loop(List(List(from)), Set.empty).reverse
  }

  override def toString: String = {
    graph.edges map { case e => e.from + " -> " + e.to } mkString "\n"
  }

  def getPathTree: Tree[Expr] = {
    def buildTree(node: Expr, used: List[Expr]): Tree[Expr] = {
      val children = graph.getSuccessors(node)
        .filterNot(used.contains)
        .map(child => buildTree(child, child :: used))

      Tree(node, children)
    }

    buildTree(True, Nil)
  }
}