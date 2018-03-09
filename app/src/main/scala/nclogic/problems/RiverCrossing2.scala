package nclogic.problems

import nclogic.model.HistoryGraph
import nclogic.model.converters.CnfConverter
import nclogic.model.expr.Expr._
import nclogic.model.expr.{C, _}
import nclogic.sat.Sat

object RiverCrossing2 {

  val policeman = Term("policeman")
  val thief = Term("thief")
  val mother = Term("mother")
  val father = Term("father")
  val boy1 = Term("boy1")
  val boy2 = Term("boy2")
  val girl1 = Term("girl1")
  val girl2 = Term("girl2")
  val boat = Term("boat")


  val goRight = (x: Term) => !x & N(x)
  val goLeft = (x: Term) => x & N(!x)

  val problem: Expr = and(
    (thief <-> policeman) | and(boy1 <-> !thief, boy2 <-> !thief, girl1 <-> !thief, girl2 <-> !thief, mother <-> !thief, father <-> !thief)
    , N((thief <-> policeman) | and(boy1 <-> !thief, boy2 <-> !thief, girl1 <-> !thief, girl2 <-> !thief, mother <-> !thief, father <-> !thief))
    , (mother <-> father) | ((boy1 <-> !mother) & (boy2 <-> !mother))
    , N((mother <-> father) | ((boy1 <-> !mother) & (boy2 <-> !mother)))
    , (mother <-> father) | ((girl1 <-> !father) & (girl2 <-> !father))
    , N((mother <-> father) | ((girl1 <-> !father) & (girl2 <-> !father)))
    , boat -> !(goRight(policeman) | goRight(thief) | goRight(father) | goRight(mother) | goRight(boy1) | goRight(boy2) | goRight(girl1) | goRight(girl2))
    , !boat -> !(goLeft(policeman) | goLeft(thief) | goLeft(father) | goLeft(mother) | goLeft(boy1) | goLeft(boy2) | goLeft(girl1) | goLeft(girl2))

    , C(boat)
    , C(policeman) | C(mother) | C(father)
    , C(girl1) -> C(policeman) | C(mother) | C(father)
    , C(girl2) -> C(policeman) | C(mother) | C(father)
    , C(boy1) -> C(policeman) | C(mother) | C(father)
    , C(boy2) -> C(policeman) | C(mother) | C(father)
    , C(thief) -> C(policeman)

    /////////////////////////////////////////////////
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
    , (C(mother) & C(father)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman))

    , (C(father) & C(boy1)) -> (!C(thief) & !C(boy2) & !C(girl1) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(boy2)) -> (!C(thief) & !C(boy1) & !C(girl1) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(girl1)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl2) & !C(policeman) & !C(mother))
    , (C(father) & C(girl2)) -> (!C(thief) & !C(boy1) & !C(boy2) & !C(girl1) & !C(policeman) & !C(mother))
  )

  val start = !boat & !policeman & !thief & !boy1 & !boy2 & !girl1 & !girl2 & !mother & !father
  val end = policeman & thief & boy1 & boy2 & girl1 & girl2 & mother & father

  def main(args: Array[String]): Unit = {
    val cnf = CnfConverter.convert(problem)
    val sat = Sat.solve(cnf)
    val graph = HistoryGraph(sat)
    val path = graph.findPath(start, end)
    path foreach { x => {
      val es = x.asInstanceOf[And].es
      es.filter(_.isInstanceOf[Neg]).foreach( x => print(x + " "))
      print("  -----   ")
      es.filterNot(_.isInstanceOf[Neg]).foreach( x => print(x + " "))
      println()
    }}
    println(path.size)
  }

}
