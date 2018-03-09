package nclogic.problems

import nclogic.model.HistoryGraph
import nclogic.model.expr.Expr._
import nclogic.model.expr.{C, Expr, N, Term}
import nclogic.sat.Sat

object RiverCrossing {

  val policeman = Term("policeman")
  val thief = Term("thief")
  val mother = Term("mother")
  val father = Term("father")
  val boy1 = Term("boy1")
  val boy2 = Term("boy2")
  val girl1 = Term("girl1")
  val girl2 = Term("girl2")
  val boat = Term("boat")


  val problem: Expr = and(
    C(boat)
    , (thief <-> policeman) | ((boy1 & boy2 & girl1 & girl2 & mother & father) <-> !thief)
    , N((thief <-> policeman) | ((boy1 & boy2 & girl1 & girl2 & mother & father) <-> !thief))
    , (mother <-> father) | ((boy1 & boy2) <-> !mother)
    , N((mother <-> father) | ((boy1 & boy2) <-> !mother))
    , (mother <-> father) | ((girl1 & girl1) <-> !father)
    , N((mother <-> father) | ((girl1 & girl1) <-> !father))

    , C(policeman) | C(mother) | C(father)

    , C(girl1) -> C(policeman) | C(mother) | C(father)
    , C(girl2) -> C(policeman) | C(mother) | C(father)
    , C(boy1) -> C(policeman) | C(mother) | C(father)
    , C(boy2) -> C(policeman) | C(mother) | C(father)
    , C(thief) -> C(policeman) | C(mother) | C(father)

    , (C(policeman) & C(thief)) -> (!C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(mother) & !C(father))
    , (C(policeman) & C(boy1)) -> (!C(thief) & !C(boy2) & !C(girl1) & !C(girl2) & !C(mother) & !C(father))
    , (C(policeman) & C(boy2)) -> (!C(thief) & !C(boy1) & !C(girl1) & !C(girl2) & !C(mother) & !C(father))
    , (C(policeman) & C(girl1)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl2) & !C(mother) & !C(father))
    , (C(policeman) & C(girl2)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(mother) & !C(father))
    , (C(policeman) & C(mother)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(father))
    , (C(policeman) & C(father)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(mother))

    , (C(mother) & C(boy1)) -> (!C(thief) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman) & !C(father))
    , (C(mother) & C(boy2)) -> (!C(thief) & !C(boy1) & !C(girl1) & !C(girl2) & !C(policeman) & !C(father))
    , (C(mother) & C(girl1)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl2) & !C(policeman) & !C(father))
    , (C(mother) & C(girl2)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(policeman) & !C(father))
    , (C(mother) & C(mother)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(father))
    , (C(mother) & C(father)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman))

    , (C(father) & C(boy1)) -> (!C(thief) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(boy2)) -> (!C(thief) & !C(boy1) & !C(girl1) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(girl1)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(girl2)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(policeman) & !C(mother))
    , (C(father) & C(mother)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(mother))
    , (C(father) & C(father)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman))
  )

  val start = !boat & !policeman & !thief & !boy1 & !boy2 & !girl1 & !girl2 & !mother & !father

  def main(args: Array[String]): Unit = {
    val cnf = problem.cnf
    val sat = Sat.solve(cnf)
    val graph = HistoryGraph(sat)
    var succ = graph.getSuccessors(start)
    succ = graph.getSuccessors(succ.head)
    succ foreach println
    println(succ.size)
  }

}
