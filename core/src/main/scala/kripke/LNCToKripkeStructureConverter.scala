package kripke

import bool.{BoolSat, BoolSatIterator}
import nclogic.mc.LNCMC
import nclogic.model.expr.{Expr, _}
import nclogic.model.{LNC, NormalFormConverter, PrefixFormulaConverter}
import nclogic.sat.TableAux

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

  def convert(formula: Expr, satSolver: BoolSat): KripkeStructure = {
    val d = LNC.depth(formula)
    val vars = LNCMC.getVars(formula)
    val allVars =
      (for {
        i <- 0 to d
        v <- vars
      } yield {
        N(i, v)
      }).toSet

    val (normal, prefitTime) = time.measureTime {
      PrefixFormulaConverter.convert3(formula)
    }

    println(prefitTime.seconds)

//    val all = satSolver.getAllSolutions(normal)

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


    kripke
  }

  def findPathBFS(formula: Expr, from: Set[Expr], to: Set[Expr], satSolver: BoolSat): Option[List[Set[Expr]]] = {
    val prefix = PrefixFormulaConverter.convert3(formula)
    val baseVars = LNCMC.getVars(formula)

    def getNext(node: Set[Expr], visited: Set[Set[Expr]]): Set[Set[Expr]] = {
      val simplified = TableAux.setTrue(prefix, node)

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

  def findPathDFS(formula: Expr, from: Set[Expr], to: Set[Expr], satSolver: BoolSat): Option[List[Set[Expr]]] = {
    val prefix = PrefixFormulaConverter.convert3(formula)
    val baseVars = LNCMC.getVars(formula)

    def getIterator(node: Set[Expr]): BoolSatIterator = {
      val simplified = TableAux.setTrue(prefix, node)
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

              case Some(next) if path.contains(next) =>
                dfs(nodes)

              case Some(next) =>
                if (!itMap.contains(next)) {
                  itMap.put(next, (false, getIterator(next)))
                }
                dfs((next :: path) :: nodes)
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
}
