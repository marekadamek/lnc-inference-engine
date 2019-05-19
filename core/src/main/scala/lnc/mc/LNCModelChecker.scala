package lnc.mc

import lnc.LNC
import lnc.bool.BoolSat
import lnc.expr.Expr._
import lnc.expr._
import lnc.kripke.{KripkeStructure, KripkeStructureNode}

/**
  * Kripke structure paths iterator
  * @param model Kripke structure
  * @param initialNodes initial nodes
  * @param d length of paths
  */
class PathIterator(model: KripkeStructure, initialNodes: Set[KripkeStructureNode], d: Int) extends Iterator[List[KripkeStructureNode]] {
  private var toDo = initialNodes.map(List(_)).toList

  override def hasNext: Boolean = toDo.nonEmpty

  override def next(): List[KripkeStructureNode] = {
    var path = toDo.head

    while (path.length < d) {
      val newPaths = model.getSuccessors(path.head.id).map(_ :: path).toList
      toDo = newPaths ::: toDo.tail
      path = toDo.head
    }

    toDo = toDo.tail
    path.reverse
  }
}

/**
  * Solves Model Checking problem for given model represented as Kripke strukture and spec as LNC formula
  */
object LNCModelChecker {

  /**
    * Gets the set of reachable nodes in given Kripke structure
    * @param model kripke structure
    * @return set of reachable nodes
    */
  private def getReachableNodes(model: KripkeStructure): Set[KripkeStructureNode] = {
    var reachMap = model.getNodes.map(n => (n.id, false)).toMap

    var toDo = model.getInitialNodes.map(_.id).toList

    while (toDo.nonEmpty) {
      val nodeId = toDo.head
      toDo = toDo.tail

      if (!reachMap(nodeId)) {
        reachMap = reachMap.updated(nodeId, true)
        val succIds = model.edges(nodeId)
        toDo = succIds.toList ::: toDo
      }
    }

    reachMap.filter(_._2).keys.map(model.nodes.apply).toSet
  }

  /**
    * Removes terms from model that are not present in spec formula
    * @param model model under verification
    * @param terms terms present in spec formula
    * @return
    */
  private def fitToSpec(model: KripkeStructure, terms: Set[Expr]) = {
    val nodes = model.nodes.mapValues(node => {
      var common = node.terms.intersect(terms)
      if (common.isEmpty) {
        common = Set(True)
      }
      node.copy(terms = common)
    })

    new KripkeStructure(nodes, model.edges)
  }

  private def getPathIterator(model: KripkeStructure, depth: Int, vars: Set[Expr]): PathIterator = {
    val modelFit = fitToSpec(model, vars)
    val reachableNodes = getReachableNodes(modelFit)

    new PathIterator(modelFit, reachableNodes, depth + 1)
  }

  /**
    * Converts Kripke structure to LNC formula which represents all/subset possible subpaths present in the model
    * @param it path iterator
    * @param pathsAtOnce optional. If defined converts  a subset of subpaths. If not defined all subpaths are converted
    * @return LNC formula
    */
  private def convertModelToFormula(it: PathIterator, pathsAtOnce: Option[Int]): Option[Expr] = {
    val paths = pathsAtOnce match {
      case Some(n) => it.take(n).toSet
      case None => it.toSet
    }

    if (paths.isEmpty) {
      None
    } else {
      val ors = paths.map(path =>
        and(path.zipWithIndex
          .map { case (node, idx) =>
            N(idx, and(node.terms))
          }
          .toSet)
      )

      Some(or(ors))
    }
  }

  def getVars(expr: Expr): Set[Expr] = expr match {
    case True => Set.empty
    case v: Var => Set(v)
    case Not(e) => getVars(e)
    case Next(e, _) => getVars(e)
    case Change(e, _) => getVars(e)
    case And(es) => es.flatMap(getVars)
    case Or(es) => es.flatMap(getVars)
    case Impl(e1, e2) => getVars(e1) ++ getVars(e2)
    case Eq(e1, e2) => getVars(e1) ++ getVars(e2)
  }

  /**
    * Verifies if model given as Kripke Structure satisfies spec given as LNC formula
    * @param model model under verification
    * @param specification LNC formula
    * @param satSolver helper booelan SAT solver
    * @param pathsAtOnce optional. If defined converts  a subset of subpaths. If not defined all subpaths are converted
    * @return if model satisfies specification is it None. Otherwise LNC formula representing counterexample is returned
    */
  def verify(model: KripkeStructure, specification: Expr, satSolver: BoolSat, pathsAtOnce: Option[Int]): Option[Set[Expr]] = {
    val vars = getVars(specification)
    val depth = LNC.depth(specification)

    var continue = true
    var result = Option.empty[Set[Expr]]
    val it = getPathIterator(model, depth, vars)
    while (continue) {
      convertModelToFormula(it, pathsAtOnce) match {
        case None => continue = false
        case Some(modelAsFormula) =>
          val finalFormula = modelAsFormula & !specification
          satSolver.getSolution(finalFormula) match {
            case Some(e) =>
              result = Some(e)
              continue = false
            case None =>
          }
      }
    }

    result
  }
}
