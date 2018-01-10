package nclogic

import nclogic.lang.{Terminate, Until, Eval, Block}
import nclogic.model.expr.Expr._
import nclogic.model.expr.{Var, _}
import nclogic.model.Formula._

object Main extends App {

  val a: Var = "a"
  val b: Var = "b"
  val c: Var = "c"

  val e = and(
    a -> N(b -> N(c & Terminate))
  )

  //Eval.eval(e)
  //e.eval(Map((a, True)))


  println(LncInferenceEngine.getHistoryGraph(e))
  //  println(LncInferenceEngine.isTautology(a | !a))
  //  println(x.matches(y))
  //  println(y.matches(x))
  //println(Expr.or(x, y))
  //println(e exists until(a, c))
  //val d =  N(C(a) & b & N(c)).simplify
  //println(LncInferenceEngine.getHistoryGraph(e))

  //e.paths foreach println
  //println(LncInferenceEngine.getHistoryGraph(e).getSuccessors(Set[Expr](a, !b)))

}

