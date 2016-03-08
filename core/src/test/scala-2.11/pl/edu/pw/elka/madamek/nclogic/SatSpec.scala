package pl.edu.pw.elka.madamek.nclogic

import org.scalatest._
import Tokenizer
import pl.edu.pw.elka.madamek.nclogic.sat.Sat

class SatSpec extends FlatSpec with Matchers {

  "Sat" should "find solutions for edge cases" in {

    for {
      tokens <- Tokenizer.tokenize("(a | b) & (!a | d)")
      expr <- Parser.parse(tokens)
    } yield {
      println(Sat.solve(expr))
    }
  }
}
