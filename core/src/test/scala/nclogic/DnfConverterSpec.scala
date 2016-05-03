package nclogic

import nclogic.model.CnfConverter
import nclogic.model.Types.Var
import nclogic.parser.Parser
import org.scalatest.{FlatSpec, Matchers}

class DnfConverterSpec extends FlatSpec with Matchers {

  "CNF converter (a & b) | (c & d)" should "work" in {

    val formula = "(a & b) | (c & d)"

    for {
      expr <- Parser.parse(formula)
      cnf = CnfConverter.convert(expr)
    } {
      cnf shouldEqual Set(Set(Var("a"), Var("c")), Set(Var("a"), Var("d")), Set(Var("b"), Var("c")), Set(Var("b"), Var("d")))
    }
  }

  "CNF converter (a & b) | (c | d)" should "work" in {

    val formula = "(a & b) | (c | d)"

    for {
      expr <- Parser.parse(formula)
      cnf = CnfConverter.convert(expr)
    } {
      cnf shouldEqual Set(Set(Var("a"), Var("c"), Var("d")), Set(Var("b"), Var("c"), Var("d")))
    }
  }
}
