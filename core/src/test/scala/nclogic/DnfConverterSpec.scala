package nclogic

import nclogic.model.Types.Var
import nclogic.model.{CnfConverter, DnfConverter}
import nclogic.parser.Parser
import nclogic.sat.Sat
import org.scalatest.{Matchers, FlatSpec}

class DnfConverterSpec extends FlatSpec with Matchers {

  "DNF converter" should "work" in {

    val formula = "C(a | b)"

    for {
      expr <- Parser.parse(formula)
      dnf = CnfConverter.convert(expr)
    } {
      val graph = LncInferenceEngine.getHistoryGraph(expr)
      graph foreach println
      println(graph.getSuccessors(Set(Var("a"))))
    }

  }

  "DNF converter" should " test 2" in {

    val formula = "(a | b | c) & (d | e | f) & (g | h | i)"

    for {
      expr <- Parser.parse(formula)
      dnf = DnfConverter.convert(expr)
    } {
      println(dnf)
    }

  }
}
