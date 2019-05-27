package lnc.mc

import lnc.LNC
import lnc.bool.{BoolSat, BoolSatIterator}
import lnc.expr.Expr._
import lnc.expr._
import lnc.expr.converters.NormalFormConverter
import lnc.kripke.{KripkeStructure, KripkeStructureNode}

import scala.collection.mutable

/**
  * Kripke structure paths iterator
  *
  * @param model        Kripke structure
  * @param initialNodes initial nodes
  * @param d            length of paths
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
    *
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
    *
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

  private def getPathIterator(model: KripkeStructure, depth: Int): PathIterator = {
    //val modelFit = fitToSpec(model, vars)
    //val reachableNodes = getReachableNodes(modelFit)

    val modelFit = model
    val reachableNodes = model.nodes.values.toSet
    new PathIterator(modelFit, reachableNodes, depth + 1)
  }

  val cache = mutable.Map.empty[(Int, Int), Set[List[Set[Expr]]]]

  private def getNodePaths(model: KripkeStructure, node: Int, depth: Int): Set[List[Set[Expr]]] = cache.getOrElseUpdate((node, depth), {
    val ts = model.nodes(node).terms

    if (depth == 0) {
      Set(List(ts))
    }
    else {
      model.edges(node)
        .flatMap(s => {
          val paths = getNodePaths(model, s, depth - 1)
          paths.map(ts :: _)
        })
    }
  })

  /**
    * Converts Kripke structure to LNC formula which represents all/subset possible subpaths present in the model
    *
    * @param it          path iterator
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
            val e =
              if (node.terms.isEmpty) Expr.T
              else and(node.terms)
            N(idx, e)
          }
          .toSet)
      )

      Some(NormalFormConverter.convertToNormalForm(or(ors)))
    }
  }


  def getTransitionFunction(model: KripkeStructure): Expr = {
    val transitions = model.edges.flatMap {
      case (n, susseccors) =>
        val from = model.nodes(n).terms
        susseccors.map(s => {
          val to = model.nodes(s).terms.map(N)
          Expr.and(from ++ to)
        })
    }

    Expr.or(transitions.toSet)
  }

  def getVars(expr: Expr): Set[Expr] = expr match {
    case True => Set.empty
    case False => Set.empty
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
    *
    * @param model       model under verification
    * @param specs       system properties given as LNC formulas
    * @param satSolver   helper booelan SAT solver
    * @param pathsAtOnce optional. If defined converts  a subset of subpaths. If not defined all subpaths are converted
    * @return if model satisfies specification is it None. Otherwise LNC formula representing counterexample is returned
    */
  def verify(model: KripkeStructure, specs: List[Expr], satSolver: BoolSat, pathsAtOnce: Option[Int]): List[Option[Set[Expr]]] = {
    val depth = specs.map(LNC.depth).max


    def batchLoop(it: PathIterator, modelFormula: Expr, specNegated: Expr): (Option[Set[Expr]], Expr) = {
      convertModelToFormula(it, pathsAtOnce) match {
        case None =>
          (None, modelFormula)
        case Some(batchFormula) =>
          val newModelFormula = or(modelFormula, batchFormula)

          satSolver.getSolution(and(batchFormula, specNegated)) match {
            case Some(e) =>
              (Some(e), newModelFormula)
            case None =>
              batchLoop(it, newModelFormula, specNegated)
          }
      }
    }

    val (_, res) = specs.foldLeft((F, List.empty[Option[Set[Expr]]])) {
      case ((modelFormula, results), spec) =>
        val (result, mf) = NormalFormConverter.convertToNormalForm(not(spec)) match {
          case False => (Some(Set.empty[Expr]), modelFormula)
          case True => (None, modelFormula)
          case specNegated =>
            val it = getPathIterator(model, depth)
            //            satSolver.getSolution(Expr.and(modelFormula, specNegated)) match {
            //              case Some(e) =>
            //                (Some(e), modelFormula)
            //              case None =>
            batchLoop(it, modelFormula, specNegated)
          //   }
        }
        (mf, result :: results)
    }

    res.reverse
  }

  def verify(model: KripkeStructure, spec: Expr, satSolver: BoolSat, pathsAtOnce: Option[Int]): Option[Set[Expr]] = {
    verify(model, List(spec), satSolver, pathsAtOnce).head
  }

  def verify2(model: KripkeStructure, specs: List[Expr], satSolver: BoolSat): List[Option[Set[Expr]]] = {

    def findPrefix(node: KripkeStructureNode, minD: Int): Option[List[KripkeStructureNode]] = {
      if (minD == 0) Some(Nil)
      else {
        val it = model.edges.filter(_._2.contains(node.id)).keySet.iterator
        while (it.hasNext) {
          val predecessor = model.nodes(it.next())
          findPrefix(model.nodes(it.next()), minD - 1) match {
            case Some(prefix) =>
              return Some(predecessor :: prefix)
            case _ =>
          }
        }
        None
      }
    }

    def find(paths: List[(List[KripkeStructureNode], Set[Expr])], minD: Int): Option[List[KripkeStructureNode]] = paths match {
      case Nil => None
      case ((path, terms)) :: tail =>
        if (terms.isEmpty) {
          val candidate = path.reverse
          findPrefix(candidate.head, minD).map(prefix =>{
            prefix.reverse ++ candidate
          })
        } else {
          val (baseTerms, rest) = terms.partition(LNC.depth(_) == 0)
          if (isContradictory(path.head.terms ++ baseTerms)) {
            find(tail, minD)
          } else {

            val x = rest.map {
              case Not(Next(x, l)) => not(N(l - 1, x))
              case Next(x, l) => N(l - 1, x)
            }

            val newPaths = model.getSuccessors(path.head.id).map(_ :: path).map((_, x))

            find(newPaths.toList ::: tail, minD)
          }
        }
    }

    def loop(it: BoolSatIterator): Option[List[KripkeStructureNode]] = {
      it.next() match {
        case None => None
        case Some(sol) =>
          val minD = sol.map(LNC.depth).min
          val normalized = sol.map {
            case Next(e, l) => N(l - minD, e)
            case Not(Next(e, l)) => not(N(l - minD, e))
            case e => e
          }
          val start = model.nodes.values.toList.map(n => (List(n), normalized))

          find(start, minD) match {
            case None => loop(it)
            case some => some
          }
      }

    }

    specs.map(spec => {
      Expr.not(spec) match {
        case False => Some(Set.empty[Expr])
        case True => None
        case specNegated =>
          val it = satSolver.iterator(specNegated)
          loop(it).map(list => {
            list.zipWithIndex
              .flatMap {
                case (node, idx) =>
                  if (node.terms.isEmpty) Set(Expr.T)
                  else node.terms.map(N(idx, _))
              }
              .toSet
          })
      }
    })
  }
}
