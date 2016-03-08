package pl.edu.pw.elka.madamek.nclogic

import org.scalatest._
import Tokenizer
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
