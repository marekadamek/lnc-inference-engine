package nclogic.sat

import bool.BoolSatIterator
import nclogic.model.LNC
import nclogic.model.expr._

class BDDTree(val e: Expr) {
  var left: Option[BDDTree] = None
  var right: Option[BDDTree] = None
}

case class TableAuxBDDLNC(expr: Expr) extends BoolSatIterator {

  private def getAllTerms(toDo: List[Expr], acc: Set[Expr] = Set.empty): Set[Expr] = toDo match {
    case Nil => acc
    case head :: tail => head match {
      case True | False => getAllTerms(tail, acc)
      case Not(x) if head.isTerm => getAllTerms(tail, acc + x)
      case _ if head.isTerm => getAllTerms(tail, acc + head)
      case And(es) => getAllTerms(es.toList ::: tail, acc)
      case Or(es) => getAllTerms(es.toList ::: tail, acc)
      case Impl(e1, e2) => getAllTerms(e1 :: e2 :: tail, acc)
      case Eq(e1, e2) => getAllTerms(e1 :: e2 :: tail, acc)
      case Not(e) => getAllTerms(e :: tail, acc)
    }
  }

  def getAT(term: Expr) = term match {
    case Var(v) => v
    case Not(Var(v)) => v
    case Next(Var(v), _) => v
    case Not(Next(Var(v), _)) => v
  }

  val allTerms = getAllTerms(List(expr))
  val allAt = allTerms.filterNot(_.isInstanceOf[Not]).toArray.sortBy(t => (LNC.depth(t), getAT(t)))


  private val root = new BDDTree(expr)
  private var toDo = List((root, 0, true, List.empty[Expr]), (root, 0, false, List.empty[Expr]))


  private def advance(s: List[Expr], acc: List[Expr] = Nil): List[Expr] = s match {
    case Nil => acc.reverse
    case Var(_) :: ts => advance(ts, acc)
    case Not(Var(_)) :: ts => advance(ts, acc)
    case Next(x, l) :: ts => advance(ts, N(l - 1, x) :: acc)
    case Not(Next(x, l)) :: ts => advance(ts, Not(N(l - 1, x)) :: acc)
  }


  def traverse(node: BDDTree, solution: List[Expr], idx: Int): Unit = node.e match {
    case True | False =>

    case _ if idx < solution.length =>
      val t = solution(idx)
      applyTerm(node, t)

      val isNot = t.isInstanceOf[Not]
      val next =
        if (isNot) {
          node.left
        } else {
          node.right
        }

      toDo = (node, idx, !isNot, solution.take(idx).reverse) :: toDo
      traverse(next.get, solution, idx + 1)

    case _ =>
      toDo = (node, idx, true, solution.reverse) :: (node, idx, false, solution.reverse) :: toDo
  }

  def applyTerm(node: BDDTree, t: Expr): Option[BDDTree] = node.e match {
    case True | False => None
    case _ =>
      val bdd = Some(new BDDTree(TableAux.setTrue(node.e, Set(t))))
      t match {
        case Not(_) =>

          if (node.left.isEmpty) {
            node.left = bdd
          }
          node.left

        case _ =>
          if (node.right.isEmpty) {
            node.right = bdd
          }
          node.right
      }
  }

  var allSolutions = List.empty[List[Expr]]

  def findCycle(cycle: List[List[Expr]]): Option[List[List[Expr]]] = {
    val suffix = advance(cycle.head)

    val sl = suffix.length

    def matches(s: List[Expr]) = {
      val l = Math.min(sl, s.length)
      suffix.take(l) == s.take(l)
    }

    val found = cycle.exists(matches)

    if (found) {
      Some(cycle)
    } else {
      allSolutions.find(matches) match {
        case None => None
        case Some(next) => findCycle(next :: cycle)
      }
    }
  }

  def next(): Option[Set[Expr]] = {

    var result = Option.empty[Set[Expr]]

    while (toDo.nonEmpty && result.isEmpty) {
      val (node, idx, pos, terms) = toDo.head
      toDo = toDo.tail

      node.e match {
        case False => //skip

        case True =>
          val solution = terms.reverse
          val suffix = advance(solution)

          traverse(root, suffix, 0)

          allSolutions = solution :: allSolutions

        result = findCycle(List(solution)).map(_.head.toSet)

        case _ if pos && node.right.isEmpty || !pos && node.left.isEmpty =>
          val t =
            if (pos) {
              allAt(idx)
            }
            else {
              Not(allAt(idx))
            }

          applyTerm(node, t) match {
            case None =>
              println("should not happen!!!")
            case Some(bdd) =>
              toDo = (bdd, idx + 1, true, t :: terms) :: (bdd, idx + 1, false, t :: terms) :: toDo
          }

        case _ =>

      }
    }

    result
  }
}


