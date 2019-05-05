package lnc.mc

import lnc.LNC
import lnc.bool.{BoolSat, BoolSatIterator}
import lnc.expr.converters.{NormalFormConverter, PrefixFormulaConverter}
import lnc.expr.{Expr, N, Next, Not}
import lnc.kripke.LNCToKripkeStructureConverter.addMissingTerms
import lnc.kripke.{KripkeStructure, LNCToKripkeStructureConverter}

import scala.collection.mutable

case class LNCModel(formula: Expr, satSolver: BoolSat) {

  private val normal = NormalFormConverter.convertToNormalForm(formula)
  private val baseVars = LNCModelCheker.getVars(formula)

  def findPathBFS(from: Set[Expr], to: Set[Expr]): Option[List[Set[Expr]]] = {
    def getNext(node: Set[Expr], visited: Set[Set[Expr]]): Set[Set[Expr]] = {
      val simplified = Expr.setTrue(normal, node)

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

  def findPathDFS(from: Set[Expr], to: Set[Expr]): Option[List[Set[Expr]]] = {
    def getIterator(node: Set[Expr]): BoolSatIterator = {
      val simplified = Expr.setTrue(normal, node)
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


  def findCycle(): Option[Set[Expr]] = {
    val (prefix, solutions) = PrefixFormulaConverter.calculatePrefix(NormalFormConverter.convertToNormalForm(formula))

    val visited = mutable.Set.empty[Set[Expr]]
    val mainIt = satSolver.iterator(prefix)

    val cache = mutable.Map.empty[Set[Expr], BoolSatIterator]

    def matches(s1: Set[Expr], s2: Set[Expr]) = s1 == s2

    def getIterator(n: Set[Expr]) = cache.getOrElseUpdate(n, new BoolSatIterator {
      private val simp = Expr.setTrue(normal, n)
      private val it = satSolver.iterator(simp)

      override def next(): Option[Set[Expr]] = it.next().map(s => (s ++ n)
        .filter(LNC.depth(_) > 0)
        .map {
          case Next(e, l) => N(l - 1, e)
          case Not(Next(e, l)) => Not(N(l - 1, e))
        })
    })


    var result = Option.empty[Set[Expr]]
    var start = mainIt.next()

    while (start.nonEmpty && result.isEmpty) {

      var toDo = start
        .map(s => List(List(s)))
        .getOrElse(Nil)

      while (toDo.nonEmpty && result.isEmpty) {
        val path = toDo.head

        if (path.tail.exists(s => matches(s, path.head))) {
          result = Some(path.head)
        } else {
          if (!visited.contains(path.head)) {
            it.next() match {
              case None =>
                visited.add(path.head)
                toDo = toDo.tail
              case Some(next) =>
                val nextIt = getIterator(next)

                toDo = (next :: path, nextIt) :: toDo
            }
          } else {
            toDo = toDo.tail
          }
        }
      }

      if (result.isEmpty) {
        start = mainIt.next()
      }
    }

    result
  }

  def toKripkeStructure(satSolver: BoolSat): KripkeStructure = {
    LNCToKripkeStructureConverter.convert(formula, satSolver)
  }
}
