package lnc.mc

import lnc.bool.BoolSat
import lnc.kripke.{KripkeStructure, KripkeStructureNode}
import lnc.LNC
import lnc.expr._

import scala.collection.mutable

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

object LNCModelCheker {

  var cache = mutable.Map.empty[(Int, Int), Expr]

  private def perms(vars: List[Var], limit: Int, acc: List[List[Expr]] = List(Nil)): List[List[Expr]] = vars match {
    case Nil => acc
    case _ if acc.size >= limit => acc.take(limit)
    case v :: vs =>
      val newAcc = acc.flatMap(s => List(v :: s, Not(v) :: s))
      perms(vs, limit, newAcc)
  }

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

  private def fitToSpec(model: KripkeStructure, vars: Set[Expr]) = {
    val nodes = model.nodes.mapValues(node => {
      var terms = node.terms.intersect(vars)
      if (terms.isEmpty) {
        terms = Set(True)
      }
      node.copy(terms = terms)
    })

    new KripkeStructure(nodes, model.edges)
  }

  private def getPathIterator(model: KripkeStructure, depth: Int, vars: Set[Expr]): PathIterator = {
    val modelFit = fitToSpec(model, vars)
    val reachableNodes = getReachableNodes(modelFit)

    new PathIterator(modelFit, reachableNodes, depth + 1)
  }

  private def convertModelToFormula(it: PathIterator, pathsAtOnce: Option[Int]): Option[Expr] = {
    val paths = pathsAtOnce match {
      case Some(n) => it.take(n).toSet
      case None => it.toSet
    }

    if (paths.isEmpty) {
      None
    } else {
      val ors = paths.map(path =>
        Expr.and(path.zipWithIndex
          .map { case (node, idx) =>
            N(idx, Expr.and(node.terms))
          }
          .toSet)
      )

      Some(Expr.or(ors))
    }
  }

  private def solveMC(expr: Expr, satSolver: BoolSat): Option[Set[Expr]] = {
    satSolver.getSolution(expr)
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

  def verify(model: KripkeStructure, specification: Expr, satSolver: BoolSat, pathsAtOnce: Option[Int]): Option[Set[Expr]] = {
    println(s"Nodes: ${model.nodesCount}")
    println(s"Initial nodes: ${model.initialNodesCount}")
    println(s"Edges: ${model.edgesCount}")
    println

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
          solveMC(finalFormula, satSolver) match {
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
