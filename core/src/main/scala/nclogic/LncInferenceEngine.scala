package nclogic

import nclogic.model._
import nclogic.model.converters.CnfConverter
import nclogic.model.expr._
import nclogic.sat.Sat

object LncInferenceEngine {

  def isTautology(formula: Expr): Boolean = isContraTautology(Neg(formula))

  def isContraTautology(formula: Expr): Boolean = (formula :> CnfConverter.convert :> Sat.solve) == False

  def getHistoryGraph(formula: Expr) = HistoryGraph(formula)

 // def query(query: Expr, formula: Expr): Boolean = queryPaths(query, formula).nonEmpty

//  def queryPaths(query: Expr, formula: Expr): List[List[Expr]] = {
//    val graph = HistoryGraph(formula)
//    val querySolutions = toSet(Sat.solve(CnfConverter.convert(query)))
//
//    querySolutions.flatMap(q => matches(And(q), graph.graph.nodes, graph))
//  }

  private def getSubstiturionSets(e: Expr): List[SubstitutionSet] = {
    val predicates = e.getTerms
      .filter(t => t.isInstanceOf[Predicate])
      .map(_.asInstanceOf[Predicate])
      .groupBy(x => x.name)

    predicates flatMap { case (_, set) =>
      val withVariables = set.filter(p => p.args.exists(x => x.isInstanceOf[Var]))
      val withConstants = set.filter(p => p.args.exists(x => x.isInstanceOf[Term]))

      withVariables.flatMap(wV => withConstants.map(wC => wV.unify(wC, new SubstitutionSet())))
    } toList
  }

//  private def matches(query: Expr, nodes: Set[Expr], graph: HistoryGraph): List[List[Expr]] = {
//    val (next, current) = query.getTerms.partition(_.isInstanceOf[Next])
//    val filtered = nodes.filter(n => current.forall(e => {
//      if (n.getTerms.contains(e)) true
//      else {
//        val sets = getSubstiturionSets(n)
//        sets.exists(s => n.replaceVariables(s).getTerms.contains(e))
//      }
//    }))
//
//    if (next.isEmpty) {
//      return filtered.toList.map(n => List(n))
//    }
//
//    val nextQuery = And(next.map(_.asInstanceOf[Next].e))
//    filtered.toList.flatMap(n => {
//      val successors = graph.getSuccessors(n)
//      matches(nextQuery, successors, graph) match {
//        case Nil => Nil
//        case tails => tails.map(t => n :: t)
//      }
//    })
//  }

  def getNext(clause: Set[Expr]): Set[Expr] = clause
    .filter(_.isInstanceOf[Next])
    .map(_.asInstanceOf[Next])
    .map(_.e)

  private def toSet(dnf: Expr): List[List[Expr]] = dnf match {
    case Or(es) => es flatMap toSet
    case And(es) => List(es)
    case e => List(List(e))
  }


}
