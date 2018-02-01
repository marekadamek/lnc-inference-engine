package nclogic

import nclogic.model.HistoryTree
import nclogic.model.expr.Expr._
import nclogic.model.expr.{N, _}
import nclogic.sat.Sat

object TreeTest extends App {

  val a = Term("a")
  val b = Term("b")
  val c = Term("c")
  val d = Term("d")
  val e = Term("e")

  val formula = or(
    a & N(!a & !b & c) & N(N(d)),
    b & N(!a & !b & c) & N(N(e))
    //, b & N(d | e)
  )


 // val ex = (a & b) | (a & c)
  val ex = (a | b) & (a | N(N(e))) & N(!a) & N(!b) & N(c) & (N(N(d)) | b) & (N(N(d)) | N(N(e)))
  val expr = a | N(N(a))
  val dnf = expr.dnf
  val cnf = expr.cnf
  val sat = Sat.solve(cnf)
  val treeDNF = HistoryTree(dnf)
  val treeSAT = HistoryTree(sat)
  println("Expr: " + expr)
  println("DNF: " + dnf)
  println("CNF: " + cnf)
  println("SAT: " + sat)
  println("DNF Tree")
  treeDNF.print()
  println("SAT Tree")
  treeSAT.print()


}

