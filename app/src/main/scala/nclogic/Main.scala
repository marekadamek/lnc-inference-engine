package nclogic

import nclogic.solver.DnfConverter


object Main extends App {


  val line = "C(a)"

  val expr = for {
    tokens <- Tokenizer.tokenize(line)
    expr <- Parser.parse(tokens)
  } yield expr

  
  val dnf = DnfConverter.convert(expr.get.simplify)


  var current = HistoryGraphFactory.create(dnf).head.successors
  for (i <-0 until 10) {
    println(current.head)
    current = current.head.successors
  }
}

