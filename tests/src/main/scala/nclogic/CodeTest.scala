package nclogic

import nclogic.model.HistoryGraph
import nclogic.model.converters.CnfConverter
import nclogic.java.model.expr._
import nclogic.sat.Sat

object CodeTest extends App {

  val a = Var("a")
  val b = Var("b")

  val formula = (!a -> N(N(b))) & (!b -> N(N(a)))
  val from = !a & !b & N(a) & N(b)//a

  val sat = Sat.solve(CnfConverter.convert(formula))
  val graph = HistoryGraph(sat)


  def getCycles(from: Expr): List[List[Expr]] = {
    def loop(todo: List[List[Expr]], acc: List[List[Expr]]): List[List[Expr]] = todo match {
      case Nil => acc
      case curr :: rest =>
        val newPaths = graph.getSuccessors(curr.head)
          .filterNot(s => s != from && curr.contains(s))
          .map(_ :: curr)

        val (newAcc, newTodo) = newPaths.partition(_.head == from)
        loop(rest ++ newTodo, (acc ++ newAcc.map(_.reverse)).distinct)
    }

    loop(List(List(from)), Nil)
  }

  val x = getCycles(from)

  println(x.length)
  x foreach println

}

