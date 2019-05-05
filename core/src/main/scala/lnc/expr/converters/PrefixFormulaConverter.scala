package lnc.expr.converters

import lnc.LNC
import lnc.expr.{And, Eq, Expr, False, Impl, N, Not, Or, True, Var}

object PrefixFormulaConverter {

  def calculatePrefix(ln: Expr): (Expr, Set[Set[Expr]]) = {
    val d = LNC.depth(ln)
    val reversed = LNC.reverse(ln, d)
    val (solved, solutions) = solveBaseLevel(reversed)
    (LNC.reverse(solved), solutions)
  }

  private def solutionsToExpr(solutions: Set[Set[Expr]]): Expr = {
    if (solutions.isEmpty) {
      False
    } else {
      Expr.or(solutions.map(Expr.and))
    }
  }
  def prefixFormula(ln: Expr): Expr = {
    val d = LNC.depth(ln)

    var p = ln
    val prefixes = for (i <- 1 to d) yield {
      val (pp, _) = calculatePrefix(p)
      p = pp

      N(i, p)
    }

    Expr.and(Set(ln) ++ prefixes.toSet)
  }

  private def solveBaseLevel(ln: Expr, initSolutions: Option[Set[Set[Expr]]] = None): (Expr, Set[Set[Expr]]) = ln match {
    case False => (False, Set.empty)
    case True => (True, Set(Set.empty))
    case _ =>
      val it = SuffixBDDIterator(ln, initSolutions.getOrElse(Set.empty))

      def loopSolutions(it: SuffixBDDIterator, acc: Set[Expr], solutions: Set[Set[Expr]]): (Expr, Set[Set[Expr]]) = {
        it.next() match {
          case None =>
            Expr.or(acc).simplify match {
              case True => (True, Set(Set.empty))
              case False => (False, Set.empty)
              case x => (x, solutions)
            }

          case Some((True, terms)) =>
            (True, solutions + terms)

          case Some((e, terms)) =>
            loopSolutions(it, acc + e, solutions + terms)

        }
      }

      loopSolutions(it, Set.empty, Set.empty)
  }

  private case class SuffixBDDIterator(input: Expr, solutions: Set[Set[Expr]]) {

    private var toDo = {
      if (solutions.isEmpty) {
        expand(input, List.empty)
      } else {
        solutions.toList.flatMap(sol => {
          Expr.setTrue(input, sol) match {
            case True =>
              List((True, sol.toList))
            case False =>
              Nil
            case e1 =>
              expand(e1, sol.toList) match {
                case Nil => List((e1, sol.toList))
                case nodes => nodes
              }
          }
        })
      }
    }

    def getTerm(expr: Expr): Option[Expr] = {
      def getTermLoop(list: List[Expr]): Option[Expr] = list match {
        case Nil => None
        case head :: tail => head match {
          case v: Var => Some(v)
          case Not(Var(v)) => Some(head)
          case Not(x) => getTermLoop(x :: tail)
          case And(es) => getTermLoop(es.toList ::: tail)
          case Or(es) => getTermLoop(es.toList ::: tail)
          case Impl(e1, e2) => getTermLoop(e1 :: e2 :: tail)
          case Eq(e1, e2) => getTermLoop(e1 :: e2 :: tail)
          case _ => getTermLoop(tail)
        }
      }

      getTermLoop(List(expr))
    }

    private def setTrue(e: Expr, t: Expr) = Expr.setTrue(e, Set(t))

    private def expand(e: Expr, terms: List[Expr]): List[(Expr, List[Expr])] = getTerm(e) match {
      case None => Nil
      case Some(t) => List((e, t :: terms), (e, !t :: terms))
    }

    private def loop(): Option[(Expr, Set[Expr])] = toDo match {
      case Nil => None
      case (e, terms) :: es =>
        toDo = es

        setTrue(e, terms.head) match {
          case True => Some(True, terms.toSet)
          case False => loop()
          case e1 =>
            expand(e1, terms) match {
              case Nil => Some(e1, terms.toSet)
              case nodes =>
                toDo = nodes ::: es
                loop()
            }
        }
    }

    def next(): Option[(Expr, Set[Expr])] = loop()
  }
}
