package lnc.mc

import lnc.LNC
import lnc.bool.{BoolSat, BoolSatIterator}
import lnc.expr.Expr.isContradictory
import lnc.expr.converters.NormalFormConverter
import lnc.expr.{Expr, N, Next, Not}
import lnc.kripke.LNCToKripkeStructureConverter.addMissingTerms
import lnc.kripke.{KripkeStructure, LNCToKripkeStructureConverter}
import Expr._
import scala.collection.mutable

/**
  * Rpresents symbolic model described by LNC formula
  * @param formula input formula
  * @param satSolver helper SAT solver
  */
case class LNCModel(formula: Expr, satSolver: BoolSat) {

  private val normal = NormalFormConverter.convertToNormalForm(formula)
  private val baseVars = LNCModelChecker.getVars(formula)

  /**
    * Finds path between two nodes using BFS
    * @param from star node
    * @param to goal node
    * @return path if exists
    */
  def findPathBFS(from: Set[Expr], to: Set[Expr]): Option[List[Set[Expr]]] = {
    def getNext(node: Set[Expr], visited: Set[Set[Expr]]): Set[Set[Expr]] = {
      val simplified = setTrue(normal, node)

      satSolver.getAllSolutions(simplified).map(_.filter(LNC.depth(_) > 0) map {
        case Next(e, l) => N(l - 1, e)
        case Not(Next(e, l)) => Not(N(l - 1, e))
      })
    }

    def bfs(nodes: List[List[Set[Expr]]], visited: Set[Set[Expr]] = Set.empty): Option[List[Set[Expr]]] = nodes match {
      case Nil => None
      case path :: tail =>
        val found = to.forall(path.head.contains)
        if (found) Some(path.reverse)
        else {
          if (visited.contains(path.head)) {
            bfs(tail, visited)
          } else {
            val next = getNext(path.head, visited)
            val newPaths = next
              .flatMap(addMissingTerms(_, baseVars))
              .map(_ :: path)

            bfs(tail ++ newPaths, visited + path.head)
          }
        }
    }


    val init = addMissingTerms(from, baseVars).map(List(_)).toList
    bfs(init)
  }

  /**
    * Finds path between two nodes using DFS
    * @param from star node
    * @param to goal node
    * @return path if exists
    */
  def findPathDFS(from: Set[Expr], to: Set[Expr]): Option[List[Set[Expr]]] = {
    def getIterator(node: Set[Expr]): BoolSatIterator = {
      val simplified = setTrue(normal, node)
      satSolver.iterator(simplified)
    }

    val itMap = mutable.Map.empty[Set[Expr], (Boolean, BoolSatIterator)]

    def getNext(it: BoolSatIterator): Option[Set[Expr]] = {
      it.next().map(_.filter(LNC.depth(_) > 0) map {
        case Next(e, l) => N(l - 1, e)
        case Not(Next(e, l)) => Not(N(l - 1, e))
      })
    }

    def dfs(nodes: List[List[Set[Expr]]], visited: Set[Set[Expr]] = Set.empty): Option[List[Set[Expr]]] = nodes match {
      case Nil => None
      case path :: tail =>
        val found = to.forall(path.head.contains)
        if (found) Some(path.reverse)
        else {
          val (processed, it) = itMap(path.head)
          if (processed) {
            dfs(tail)
          } else {
            getNext(it) match {
              case None =>
                itMap.update(path.head, (true, it))
                dfs(tail)

              case Some(next) =>
                val allNext = addMissingTerms(next, baseVars).filterNot(path.contains)

                allNext.filterNot(itMap.contains).foreach { n =>
                  itMap.put(n, (false, getIterator(n)))
                }

                dfs(allNext.map(_ :: path).toList ::: nodes)
            }
          }
        }
    }


    val init = addMissingTerms(from, baseVars)
    init.foreach(n => {
      itMap.put(n, (false, getIterator(n)))
    })

    dfs(init.map(List(_)).toList)
  }


  /**
    * Builds Kripke structure that represents all models of LNC formula
    * @param satSolver helper boolean SAT solver
    * @return Kripke structure that represents all models of LNC formula
    */
  def toKripkeStructure(satSolver: BoolSat): KripkeStructure = {
    LNCToKripkeStructureConverter.convert(formula, satSolver)
  }
}

object LNCModel {

  import NormalFormConverter._

  /**
    * Checks whether given LNC formula is satisfiable by detecting a cycle
    *
    * @param formula   - input LNC formula
    * @param satSolver - helper boolean SAT solver
    * @return found cycle if exists, otherwise None
    */
  def findCycle(formula: Expr, satSolver: BoolSat): Option[List[Set[Expr]]] = {
    val normal = convertToNormalForm(formula)
    val visited = mutable.Set.empty[Set[Expr]]
    val mainIt = satSolver.iterator(normal)
    val cache = mutable.Map.empty[Set[Expr], BoolSatIterator]

    def getIterator(n: Set[Expr]) = cache.getOrElseUpdate(n, new BoolSatIterator {
      private val simp = setTrue(normal, n)
      private val it = satSolver.iterator(simp)

      override def next(): Option[Set[Expr]] = it.next().map(s => (s ++ n)
        .filter(LNC.depth(_) > 0)
        .map {
          case Next(e, l) => N(l - 1, e)
          case Not(Next(e, l)) => Not(N(l - 1, e))
        })
    })

    var start = mainIt.next()

    while (start.nonEmpty) {
      var toDo = List(List(start.get))

      while (toDo.nonEmpty) {
        val path = toDo.head

        if (path.tail.exists(s => !isContradictory(s ++ path.head))) {
          return Some(path.reverse)
        } else {
          if (!visited.contains(path.head)) {
            getIterator(path.head).next() match {
              case None =>
                visited.add(path.head)
                toDo = toDo.tail
              case Some(next) =>
                toDo = (next :: path) :: toDo
            }
          } else {
            toDo = toDo.tail
          }
        }
      }

      start = mainIt.next()
    }

    None
  }
}