package lnc.kripke

import lnc.LNC._
import lnc.bool.BoolSat
import lnc.expr.converters.NormalFormConverter._
import lnc.expr.{Expr, _}
import lnc.mc.LNCModelChecker

import scala.collection.mutable

object LNCToKripkeStructureConverter {
  private def perms(vars: List[Expr], acc: List[List[Expr]] = List(Nil)): List[List[Expr]] = vars match {
    case Nil => acc
    case v :: vs =>
      val newAcc = acc.flatMap(s => List(v :: s, !v :: s))
      perms(vs, newAcc)
  }


  def addMissingTerms(terms: Set[Expr], allTerms: Set[Expr]): Set[Set[Expr]] = {
    def getTerm(e: Expr) = e match {
      case Var(_) | Next(Var(_), _) => e
      case Not(v) => v
    }

    val used = (terms - True).map(getTerm)
    val missing = allTerms -- used
    perms(missing.toList) match {
      case Nil => Set(terms)
      case ps => ps.map(terms ++ _).toSet
    }
  }

  /**
    * Recursively removes nodes that do not have successors
    *
    * @param kripke structure to be trimmed
    * @return pruned Kripke structure
    */
  private def prune(kripke: KripkeStructure): KripkeStructure = {
    val toRemove = kripke.edges.filter(_._2.isEmpty).keys
    if (toRemove.isEmpty) {
      kripke
    } else {
      val trimmed = toRemove.foldLeft(kripke)((kripke, nodeIdx) => kripke.removeNode(nodeIdx))
      prune(trimmed)
    }
  }

  private def stripTemporal(kripke: KripkeStructure): KripkeStructure = {
    val nodes = kripke.nodes.mapValues(n => n.copy(terms = n.terms.filter(depth(_) == 0)))
    new KripkeStructure(nodes, kripke.edges)
  }

  /**
    * Builds Kripke structure that represents all models of LNC formula
    *
    * @param formula   input LNC formula
    * @param satSolver helper boolean SAT solver
    * @return Kripke structure that represents all models of LNC formula
    */
  def convert(formula: Expr, satSolver: BoolSat): KripkeStructure = {
    val all = satSolver.getAllSolutions(convertToNormalForm(formula))

    var lastId = 1
    val nodeMap = mutable.Map.empty[Set[Expr], Int]

    var kripke = new KripkeStructure()

    def getOrAddNode(set: Set[Expr]): Int = {
      nodeMap.getOrElseUpdate(set, {
        val terms = set
        val node = KripkeStructureNode(lastId, terms, initial = true)
        kripke = kripke.addNode(node)
        lastId += 1
        node.id
      })
    }

    all.foreach(getOrAddNode)

    var nodes = kripke.nodes.keySet.toList
    var visited = Set.empty[Int]
    while (nodes.nonEmpty) {
      val node :: tail = nodes
      nodes = tail
      if (!visited.contains(node)) {
        visited += node

        val suffix = kripke.nodes(node).terms.filter(depth(_) > 0).map {
          case Next(e, l) => N(l - 1, e)
          case Not(Next(e, l)) => !N(l - 1, e)
        }

        all
          .map(_ ++ suffix)
          .filterNot(s => {
            Expr.isContradictory(s)
          })
          .foreach { next =>
            val to = getOrAddNode(next)
            kripke = kripke.addEdge(node, to)
            nodes = to :: nodes
          }
      }
    }

    val pruned = prune(kripke)
    val structure = stripTemporal(pruned)
    structure
  }

  def convertFull(formula: Expr, satSolver: BoolSat): KripkeStructure = {
    val (modelFormula, d) = depth(formula) match {
      case 0 => (formula & N(formula), 1)
      case i => (formula, i)
    }

    val vars = LNCModelChecker.getVars(formula)
    val allVars =
      (for {
        i <- 0 to d
        v <- vars
      } yield {
        N(i, v)
      }).toSet

    val all = satSolver.getAllSolutions(convertToNormalForm(modelFormula))
    val valuations = all.flatMap(addMissingTerms(_, allVars))

    var lastId = 1
    val nodeMap = mutable.Map.empty[Set[Expr], Int]

    var kripke = new KripkeStructure()

    def getOrAddNode(set: Set[Expr]): Int = {
      nodeMap.getOrElseUpdate(set, {
        val terms = set.filter(depth(_) == 0)
        val node = KripkeStructureNode(lastId, terms, initial = true)
        kripke = kripke.addNode(node)
        lastId += 1
        node.id
      })
    }

    valuations.foreach(v => {
      val from = v.filter(depth(_) < d)
      val to = v.filter(depth(_) > 0).map {
        case Next(e, l) => N(l - 1, e)
        case Not(Next(e, l)) => !N(l - 1, e)
      }

      val fromId = getOrAddNode(from)
      val toId = getOrAddNode(to)

      kripke = kripke.addEdge(fromId, toId)
    })

    prune(kripke)
  }
}
