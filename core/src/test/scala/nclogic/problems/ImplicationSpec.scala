package nclogic.problems

import nclogic.LncInferenceEngine
import nclogic.model.DnfConverter
import nclogic.model.Types.{Neg, Var}
import nclogic.parser.{FormulaReader, Parser}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ImplicationSpec extends FlatSpec with Matchers {

  "LNC Inference engine" should "solve farmer dilemma" in {
    val source = Source.fromURL(getClass.getResource("/problems/implication.txt"))
    val formula = Parser.parse(FormulaReader.read(source))
    val graph = LncInferenceEngine.getHistoryGraph(formula.get)

    val from = DnfConverter.convert(Parser.parse("a & !b & !c").get).head
    val to = DnfConverter.convert(Parser.parse("c").get).head

    val path = graph.findPath(from, to)

    path shouldEqual List(
      Set(Var("a"), Neg(Var("b")), Neg(Var("c"))),
      Set(Var("a"), Var("b"), Neg(Var("c"))),
      Set(Var("a"), Var("b"), Var("c"))
    )
  }
}
