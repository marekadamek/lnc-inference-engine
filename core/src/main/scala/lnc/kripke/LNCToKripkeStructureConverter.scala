package lnc.kripke

import lnc.bool.BoolSat
import lnc.LNC
import lnc.expr.converters.NormalFormConverter
import lnc.mc.LNCModelCheker
import lnc.expr.{Expr, _}

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

  private def trim(kripke: KripkeStructure): KripkeStructure = {
    val toRemove = kripke.edges.filter(_._2.isEmpty).keys
    if (toRemove.isEmpty) {
      kripke
    } else {
      val trimmed = toRemove.foldLeft(kripke)((kripke, nodeIdx) => kripke.removeNode(nodeIdx))
      trim(trimmed)
    }
  }

  def convert(formula: Expr, satSolver: BoolSat): KripkeStructure = {
    val d = LNC.depth(formula)
    val vars = LNCModelCheker.getVars(formula)
    val allVars =
      (for {
        i <- 0 to d
        v <- vars
      } yield {
        N(i, v)
      }).toSet

    val all = satSolver.getAllSolutions(NormalFormConverter.convertToNormalForm(formula))

    val valuations = all.flatMap(addMissingTerms(_, allVars))

    var lastId = 1
    val nodeMap = mutable.Map.empty[Set[Expr], Int]
    var kripke = new KripkeStructure()

    def getOrAddNode(set: Set[Expr]): Int = {
      nodeMap.getOrElseUpdate(set, {
        val terms = set.filter(LNC.depth(_) == 0)
        val node = KripkeStructureNode(lastId, terms, initial = true)
        kripke = kripke.addNode(node)
        lastId += 1
        node.id
      })
    }

    valuations.foreach(v => {
      val from = v.filter(LNC.depth(_) < d)
      val to = v.filter(LNC.depth(_) > 0).map {
        case Next(e, l) => N(l - 1, e)
        case Not(Next(e, l)) => !N(l - 1, e)
      }

      val fromId = getOrAddNode(from)
      val toId = getOrAddNode(to)

      kripke = kripke.addEdge(fromId, toId)
    })


    trim(kripke)
  }

}
