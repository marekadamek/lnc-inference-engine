package lnc.mc

import lnc.LNC
import lnc.bool.{BoolSat, BoolSatIterator}
import lnc.expr._
import lnc.kripke.{KripkeStructure, KripkeStructureNode}


/**
  * Solves Model Checking problem for given model represented as Kripke strukture and spec as LNC formula
  * using method based on base formula models.
  */
object LNCModelChecker2 {

  import Expr._

  /**
    * Helper function for findSubpath
    * @param model Kripke structure
    * @param node target node
    * @param minD minimum length of the path being searched for
    * @return
    */
  private def findPrefix(model: KripkeStructure, node: KripkeStructureNode, minD: Int): Option[List[KripkeStructureNode]] = {
    if (minD == 0) Some(Nil)
    else {
      val it = model.edges.filter(_._2.contains(node.id)).keySet.iterator
      while (it.hasNext) {
        val predecessor = model.nodes(it.next())
        findPrefix(model, model.nodes(it.next()), minD - 1) match {
          case Some(prefix) =>
            return Some(predecessor :: prefix)
          case _ =>
        }
      }
      None
    }
  }

  /**
    * Function which look for a path from initial node to given node
    * @param model Kripke structure
    * @param paths target node
    * @param minD minimum length of the path being searched for
    * @return path from initial node to given target node
    */
  private def findSubpath(model: KripkeStructure, paths: List[(List[KripkeStructureNode], Set[Expr])], minD: Int): Option[List[KripkeStructureNode]] = paths match {
    case Nil => None
    case ((path, terms)) :: tail =>
      if (terms.isEmpty) {
        val candidate = path.reverse
        findPrefix(model, candidate.head, minD).map(prefix =>{
          prefix.reverse ++ candidate
        })
      } else {
        val (baseTerms, rest) = terms.partition(LNC.depth(_) == 0)
        if (isContradictory(path.head.terms ++ baseTerms)) {
          findSubpath(model, tail, minD)
        } else {

          val x = rest.map {
            case Not(Next(x, l)) => not(N(l - 1, x))
            case Next(x, l) => N(l - 1, x)
          }

          val newPaths = model.getSuccessors(path.head.id).map(_ :: path).map((_, x))

          findSubpath(model, newPaths.toList ::: tail, minD)
        }
      }
  }

  /**
    * Main checker function
    * @param model Kripke structure
    * @param specs specification given as list of LNC formulas
    * @param satSolver solver used for iteration over negated base formula models
    * @return
    */
  def verify(model: KripkeStructure, specs: List[Expr], satSolver: BoolSat): List[Option[Set[Expr]]] = {
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

          findSubpath(model, start, minD) match {
            case None => loop(it)
            case some => some
          }
      }

    }

    specs.map(spec => {
      not(spec) match {
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
