package nclogic

import eval.EvalEngine
import nclogic.model.expr.Expr._
import nclogic.model.expr.{N, _}

object Sum extends App {
  var input = List(1, 2, 3, 4, 5)

  var sum = 0

  val hasNext = Cond("hasNext", () => input.nonEmpty)
  val addNext = Block("addNext", () => {
    sum = sum + input.head
    input = input.tail
  })

  val printResult = Block("printResult", () => println(sum))

  val sumExpr = or(
    hasNext & addNext,
    !hasNext & printResult & Terminate
  )

  println(input)
  EvalEngine.eval(sumExpr)


  //val g = LncInferenceEngine.getHistoryGraph(calc)
  //println(g.prettyString)
}

