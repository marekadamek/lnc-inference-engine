package nclogic

import nclogic.model.DnfConverter
import nclogic.parser.{Parser, Tokenizer}


object Main extends App {


  val line = "!C(a)"

  val expr = for {
    tokens <- Tokenizer.tokenize(line)
    expr <- Parser.parse(tokens)
  } yield expr


  val dnf = expr.get.simplify :> DnfConverter.convert

  println(line)

  LncInferenceEngine.getHistoryGraph(expr.get) :> println

  LncInferenceEngine.getPositiveValuations(expr.get) :> println
  LncInferenceEngine.getNegativeValuations(expr.get) :> println

  LncInferenceEngine.isTautology(expr.get) :> println
  LncInferenceEngine.isContraTautology(expr.get) :> println
  //LncInferenceEngine.getPositiveValuations(expr.get) :> println
  //LncInferenceEngine.getNegativeValuations(expr.get) :> println


}

