package nclogic.problems

import nclogic.LncInferenceEngine
import nclogic.model.{HistoryGraph, CnfConverter, DnfConverter}
import nclogic.parser.{Parser, FormulaReader}
import nclogic.utils.PrettyPrintUtil
import org.scalatest.{Matchers, FlatSpec}

import scala.io.{Codec, Source}

class FarmerDilemma extends FlatSpec with Matchers {

  "LNC Inference engine" should "solve farmer dilemma" in {
    val source = Source.fromURL(getClass.getResource("/problems/farmerDilemma.txt"))
    val formula = Parser.parse(FormulaReader.read(source))
    val graph = LncInferenceEngine.getHistoryGraph(formula.get)

    val from = DnfConverter.convert(Parser.parse("!bc & !bf & !bs & !bw & !rc & !rf & !rs & !rw").get).head
    val to = DnfConverter.convert(Parser.parse("!bc & !bf & !bs & !bw & rc & rf & rs & rw").get).head

    val path = graph.findPath(from, to)

    PrettyPrintUtil.printPath(path)

    path.isEmpty shouldEqual false
  }
}
