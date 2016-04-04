package nclogic

import nclogic.parser.{Parser, Tokenizer}
import org.scalatest._

class LncTautologySpec extends FlatSpec with Matchers {

  def checkTautology(formula: String): Unit = {
    (for {
      tokens <- Tokenizer.tokenize(formula)
      expr <- Parser.parse(tokens)
    } yield expr :> LncInferenceEngine.isTautology).get shouldBe true
  }

  def checkContraTautology(formula: String): Unit = {
    (for {
      tokens <- Tokenizer.tokenize(formula)
      expr <- Parser.parse(tokens)
    } yield expr :> LncInferenceEngine.isContraTautology).get shouldBe true
  }

  "LNC tautologies" should "work for non-temporal tautologies" in {
    List(
      "T",
      "p <=> p",
      "T | p",
      "p => q <=> !p | q",
      "p | !p",
      "p => q <=> !q => !p",
      "((p => q) & (q => r)) => (p => r)",
      "((a | b) & (a => c) & (b => c)) => c",
      "((a & b) => c) <=> (a => (b => c))"
    ) foreach checkTautology
  }

  "LNC temporal tautologies" should "work for temporal tautologies" in {
    List(
      "N(a & b) <=> N(a) & N(b)",
      "N(a | b) <=> N(a) | N(b)",
      "N(a => b) <=> N(a) => N(b)",
      "N(a <=> b) <=> N(a) <=> N(b)",
      "!N(a) <=> N(!a)",
      "C(a) <=> C(!a)",
      "C(a & b) => (C(a) | C(b))",
      "a & !C(a) & C(b) => C(a => b)",
      "!a & !b & C(a) & C(b) => C(a & b)"
    ) foreach checkTautology
  }

  "LNC contra-tautologies" should "work for non-temporal contra tautologies" in {
    List(
      "F",
      "p <=> !p",
      "F & p",
      "(!p => !q) <=> !(q => p)",
      "!(p => q <=> !p | q)",
      "p & !p",
      "!(p => q <=> !q => !p)",
      "!(((p => q) & (q => r)) => (p => r))",
      "!(((a | b) & (a => c) & (b => c)) => c)",
      "!(((a & b) => c) <=> (a => (b => c)))"
    ) foreach checkContraTautology
  }
}
