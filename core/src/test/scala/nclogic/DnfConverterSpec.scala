package nclogic

import nclogic.model.DnfConverter
import nclogic.parser.Parser
import org.scalatest.{Matchers, FlatSpec}

class DnfConverterSpec extends FlatSpec with Matchers {

  "DNF converter" should "work" in {

    val formula = "(a | b | c) & (!a | e | f) & (g | h | i)"

    for {
      expr <- Parser.parse(formula)
      dnf = DnfConverter.convert(expr)
    } {
      println(dnf)
      println(dnf.size)
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
