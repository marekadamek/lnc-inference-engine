package nclogic

import nclogic.graph.{Edge, Graph}
import nclogic.model._
import nclogic.model.converters.CnfConverter
import nclogic.model.expr._
import nclogic.sat.Sat

object LncInferenceEngine {

  def isTautology(formula: Expr) = isContraTautology(Neg(formula))

  def isContraTautology(formula: Expr) = (formula :> CnfConverter.convert :> Sat.solve) == False

  def getHistoryGraph(formula: Expr) = HistoryGraph(formula)


  def query(query: Expr, formula: Expr): Boolean = queryPaths(query, formula).nonEmpty

  def queryPaths(query: Expr, formula: Expr): List[List[Expr]] = {
    val graph = HistoryGraph(formula)
    val querySolutions = toSet(Sat.solve(CnfConverter.convert(query)))

    querySolutions.toList.flatMap(q => matches(And(q), graph.graph.nodes, graph))
  }

  private def matches(query: Expr, nodes: Set[Expr], graph: HistoryGraph): List[List[Expr]] = {
    val (next, current) = query.getTerms.partition(_.isInstanceOf[Next])
    val filtered = nodes.filter(n => current.forall(n.getTerms.contains))

    if (next.isEmpty) {
      return filtered.toList.map(n => List(n))
    }

    val nextQuery = And(next.map(_.asInstanceOf[Next].e))
    filtered.toList.flatMap(n => {
      val successors = graph.graph.getSuccessors(n)
      matches(nextQuery, successors, graph) match {
        case Nil => Nil
        case tails => tails.map(t => n :: t)
      }
    })
  }

  def getNext(clause: Set[Expr]): Set[Expr] = clause
    .filter(_.isInstanceOf[Next])
    .map(_.asInstanceOf[Next])
    .map(_.e)

  private def toSet(dnf: Expr): Set[Set[Expr]] = dnf match {
    case Or(es) => es flatMap toSet
    case And(es) => Set(es)
    case e => Set(Set(e))

  }

  def evaluate(formula: Expr, state: Expr): Unit = {
    val graph = HistoryGraph(formula)

    def evaluate(nodes: List[Expr]): Unit = nodes match {
      case Nil => ()
      case n :: tail =>
        eval(List(n))
        evaluate(graph.graph.getSuccessors(n).toList ++ tail)
    }

    val nodes = graph.graph.nodes.filter(n => Expr.and(n, state).simplify != False)
    evaluate(nodes.toList)
  }

  private def eval(es: List[Expr]): Unit = es match {
    case Nil => ()
    case e :: tail => e match {
      case And(x) => eval(x.toList ++ tail)
      case x: Block =>
        x.b()
        eval(tail)
      case _ => eval(tail)
    }
  }
}
