package nclogic.mc

import bool.BoolSat
import kripke.{KripkeStructure, KripkeStructureNode}
import nclogic.model.LNC
import nclogic.model.expr._
import time._

import scala.collection.mutable

class PathIterator(model: KripkeStructure, initialNodes: Set[KripkeStructureNode], d: Int) extends Iterator[List[KripkeStructureNode]] {
  private var toDo = initialNodes.map(List(_)).toList

  override def hasNext: Boolean = toDo.nonEmpty

  override def next(): List[KripkeStructureNode] = {
    var path = toDo.head

    while(path.length < d) {
      val newPaths = model.getSuccessors(path.head.id).map(_ :: path).toList
      toDo = newPaths ::: toDo.tail
      path = toDo.head
    }

    toDo = toDo.tail
    path.reverse
  }
}
object LNCMC {

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


  private def convertModelToFormula(model: KripkeStructure, depth: Int, vars: Set[Expr]): Expr = {
    val reachableNodes = getReachableNodes(model)

    val stateVarsC = Math.ceil(Math.log10(reachableNodes.size) / Math.log10(2)).toInt
    val stateVars = (for (i <- 1 to stateVarsC) yield Var(s"s$i")).toList
    val stateCodes = perms(stateVars, reachableNodes.size)

    val nodeCodesMap = reachableNodes.zip(stateCodes).map {
      case (node, stateCode) => (node.id, stateCode.toSet)
    } toMap

    val states = nodeCodesMap.map {
      case (id, stateCode) => (id, Expr.and(stateCode ++ model.nodes(id).terms.intersect(vars) - True))
    }

    val ors = reachableNodes.map(node => {
      val succ = model.getSuccessors(node.id)
      val succExpr = Expr.or(succ.map(_.id).map(states.apply))
      states(node.id) & N(succExpr)
    })


    val transition = Expr.or(ors)
    val transitionFormula = Expr.and((for (i <- 0 until depth) yield N(i, transition)).toSet)

    transitionFormula
  }

  private var nodeConversionMap = Map.empty[(Int, Int), Set[List[Int]]]

  private def getPaths(nodeId: Int, model: KripkeStructure, depth: Int): Set[List[Int]] = depth match {
    case 0 => Set(List(nodeId))
    case _ =>
      nodeConversionMap.getOrElse((nodeId, depth), {
        val succ = model.edges(nodeId)
        val paths = succ.flatMap(s => getPaths(s, model, depth - 1))
          .map(nodeId :: _)

        nodeConversionMap = nodeConversionMap.updated((nodeId, depth), paths)
        paths
      })
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

  private def convertModelToFormula2(it: PathIterator): Option[Expr] = {
    it.take(100).toSet match {
      case x if x.isEmpty => None
      case paths =>
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

  def verify(model: KripkeStructure, specification: Expr, satSolver: BoolSat): Option[Set[Expr]] = {
    println(s"Nodes: ${model.nodesCount}")
    println(s"Initial nodes: ${model.initialNodesCount}")
    println(s"Edges: ${model.edgesCount}")
    println

    val vars = getVars(specification)
    val depth = LNC.depth(specification)

    val (result, time) = measureTime {
      var continue = true
      var result = Option.empty[Set[Expr]]
      val it = getPathIterator(model, depth, vars)
      while (continue) {
        convertModelToFormula2(it) match {
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

    println(s"Total time: ${time.seconds}s")
    result
  }
}
