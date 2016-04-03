package nclogic.problems

import nclogic.LncInferenceEngine
import nclogic.model.{CnfConverter, DnfConverter}
import nclogic.parser.{Parser, FormulaReader}
import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

class FarmerDilemma extends FlatSpec with Matchers {

  "LNC Inference engine" should "solve farmer dilemma" in {
    val source = Source.fromURL(getClass.getResource("/problems/farmerDilemma.txt"))
    val formula = FormulaReader.read(source)

    val expr = Parser.parse(formula)
    val dnf = CnfConverter.convert(expr.get)
    //val dnf = DnfConverter.convert(expr.get)
    //val graph = LncInferenceEngine.getHistoryGraph(expr.get)
    println(dnf)
  }
}
