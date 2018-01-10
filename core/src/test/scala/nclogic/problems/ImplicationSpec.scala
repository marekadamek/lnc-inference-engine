package nclogic.problems

import nclogic.LncInferenceEngine
import nclogic.model.converters.DnfConverter
import nclogic.parser.{FormulaReader, Parser}
//import nclogic.utils.PrettyPrintUtil
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ImplicationSpec extends FlatSpec with Matchers {

  "LNC Inference engine" should "solve simple implication example" in {
    val source = Source.fromURL(getClass.getResource("/problems/implication.txt"))
    val formula = Parser.parse(FormulaReader.read(source))
    val graph = LncInferenceEngine.getHistoryGraph(formula.get)

    val from = DnfConverter.convert(Parser.parse("a & !b & !c").get)
    val to = DnfConverter.convert(Parser.parse("c").get)

    //val path = graph.findPath(from, to)

    //path shouldEqual List(
    //  Set(Var("a"), Neg(Var("b")), Neg(Var("c"))),
    //  Set(Var("a"), Var("b"), Neg(Var("c"))),
    //  Set(Var("a"), Var("b"), Var("c"))
    //)

//    PrettyPrintUtil.printPath(path)
  }
}
