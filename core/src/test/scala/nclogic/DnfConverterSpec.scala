package nclogic

import nclogic.model.Types.{Neg, Var}
import nclogic.model.{CnfConverter, DnfConverter}
import nclogic.parser.Parser
import org.scalatest.{Matchers, FlatSpec}

class DnfConverterSpec extends FlatSpec with Matchers {

  "DNF converter" should "work" in {

    val formula = "C(a)"

    for {
      expr <- Parser.parse(formula)
      dnf = CnfConverter.convert(expr)
    } {
      val graph = LncInferenceEngine.getHistoryGraph(expr)
      graph.getSuccessors(Set(Neg(Var("a")), Neg(Var("b")))) :> println
      println(graph.pairs)
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
