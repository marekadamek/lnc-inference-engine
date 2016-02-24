package pl.edu.pw.elka.madamek.nclogic

import org.scalatest._
import pl.edu.pw.elka.madamek.nclogic.parser.{Tokenizer, Parser}
import pl.edu.pw.elka.madamek.nclogic.sat.Sat
import pl.edu.pw.elka.madamek.nclogic.solver.Solver


class SolverSpec extends FlatSpec with Matchers {

  "Solver" should "check tautology" in {
    for {
      tokens <- Tokenizer.tokenize("p => q <=> !p | q")
      expr <- Parser.parse(tokens)
    } {
      Solver.isTautology(expr) shouldEqual true
    }
  }
}
