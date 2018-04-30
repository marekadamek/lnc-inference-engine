package nclogic.problems

import nclogic.model.HistoryGraph
import nclogic.model.converters.{CnfConverter, DnfConverter}
import nclogic.parser.{FormulaReader, Parser}
import nclogic.sat.Sat
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class FarmerDilemma extends FlatSpec with Matchers {

  "LNC Inference engine" should "solve farmer dilemma" in {
    val source = Source.fromURL(getClass.getResource("/problems/farmerDilemma.txt"))
    val formula = Parser.parse(FormulaReader.read(source))

    val sat = Sat.solve(CnfConverter.convert(formula.get))

    val graph = HistoryGraph(sat)

    val from = DnfConverter.convert(Parser.parse("!f & !w & !s & !c").get)
    val to = DnfConverter.convert(Parser.parse("f & w & s & c").get)

    val path = graph.findPath(from, to)

    path.size shouldEqual 8
    path.head shouldEqual from.simplify
    path.last shouldEqual to.simplify
  }
}
