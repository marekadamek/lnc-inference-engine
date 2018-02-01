package nclogic

import eval.EvalEngine
import nclogic.model.expr.Expr._
import nclogic.model.expr.{N, _}

object CalcONP extends App {
  val operators = Set("+", "-", "*", "/")
  var input = "6 4 * 1 + 4 *".split(" ").toList

  var current: String = _
  var stack = new scala.collection.mutable.Stack[String]

  val hasNext = Cond("hasNext", () => input.nonEmpty)
  val isHandling = Cond("isHandling", () => current != null)
  val takeNext = Block("takeNext", () => {
    current = input.head
    input = input.tail
  })

  val isArgument = Cond("isArgument", () => current != null && current.forall(Character.isDigit))
  val isOperator = Cond("isOperator", () => current != null && operators.contains(current))
  val putOnStack = Block("putOnStack", () => {
    stack.push(current)
    current = null
  })

  val printResult = Block("printResult", () => println(stack.pop()))

  val calculate = Block("calculate", () => {
    val arg1 = stack.pop().toInt
    val arg2 = stack.pop().toInt
    current = (current match {
      case "+" => arg1 + arg2
      case "-" => arg1 - arg2
      case "*" => arg1 * arg2
      case "/" => arg1 / arg2
    }).toString
  })

  val handle = (isArgument | (isOperator & calculate)) & (putOnStack & Cut)
  val handling = Term("handling")
  val calc = or(
    hasNext & takeNext & Drop(hasNext & takeNext) & N(handle) & Cut,
    printResult & Terminate
  )

  println(input)
  EvalEngine.eval(calc)


  //val g = LncInferenceEngine.getHistoryGraph(calc)
  //println(g.prettyString)
}

