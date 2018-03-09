package nclogic

import nclogic.model.HistoryGraph
import nclogic.model.expr.Expr._
import nclogic.model.expr.{N, _}
import nclogic.sat.Sat

object GraphTest extends App {

  val a = Term("a")
  val b = Term("b")
  val c = Term("c")
  val d = Term("d")
  val e = Term("e")

//  val formula = or(
//    a & N(!a & !b & c) & N(N(d)),
//    b & N(!a & !b & c) & N(N(e))
//    //, b & N(d | e)
//  )


 // val ex = (a & b) | (a & c)
  val expr = C(a | b)
  val dnf = expr.dnf
  val cnf = expr.cnf
  val sat = Sat.solve(cnf)
  //val treeDNF = HistoryTree(dnf)
  val graphSAT = HistoryGraph(sat)
  val succ = graphSAT.getSuccessors(!a & b)
  println(succ)
//  println("Expr: " + expr)
//  println("DNF: " + dnf)
//  println("CNF: " + cnf)
  println("SAT: " + sat)
//  println("DNF Tree")
//  println("SAT Tree")


}

