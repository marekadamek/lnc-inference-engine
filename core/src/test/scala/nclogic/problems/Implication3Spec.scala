package nclogic.problems

import nclogic.LncInferenceEngine
import nclogic.model.converters.DnfConverter
import nclogic.parser.{FormulaReader, Parser}
//import nclogic.utils.PrettyPrintUtil
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Implication3Spec extends FlatSpec with Matchers {

  "LNC Inference engine" should "solve simple implication example 3" in {
    val source = Source.fromURL(getClass.getResource("/problems/implication3.txt"))
    val formula = Parser.parse(FormulaReader.read(source))
    val graph = LncInferenceEngine.getHistoryGraph(formula.get)

    val to = DnfConverter.convert(Parser.parse("c").get)

    val fromA = DnfConverter.convert(Parser.parse("a & !b & !aa & !bb & !c").get)
    //val pathA = graph.findPath(fromA, to)
    //PrettyPrintUtil.printPath(pathA)

    println()

    val fromB = DnfConverter.convert(Parser.parse("!a & b & !aa & !bb & !c").get)
   // val pathB = graph.findPath(fromB, to)
    //PrettyPrintUtil.printPath(pathB)
  }
}
