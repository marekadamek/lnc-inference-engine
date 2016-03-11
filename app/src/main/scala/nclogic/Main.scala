package nclogic

import nclogic.model.DnfConverter
import nclogic.parser.{Tokenizer, Parser}


object Main extends App {


  val line = "C(a & b) => C(a) & C(b)"

  val expr = for {
    tokens <- Tokenizer.tokenize(line)
    expr <- Parser.parse(tokens)
  } yield expr


  val dnf = expr.get.simplify :> DnfConverter.convert
  println(LncInferenceEngine.getPositiveValuations(expr.get))
  println(LncInferenceEngine.getNegativeValuations(expr.get))

}

