package nclogic

import nclogic.model.expr._
import org.scalatest._

class TypesSpec extends FlatSpec with Matchers {

  "Const" should "be valid" in {
    True.simplify shouldEqual True
    False.simplify shouldEqual False
  }

  "Var" should "be valid" in {
    Var("a").simplify shouldEqual Var("a")
  }

  "Neg" should "be valid" in {
    Not(True).simplify shouldEqual False
    Not(False).simplify shouldEqual True
    Not(Not(Var("a"))).simplify shouldEqual Var("a")
  }

  "Or" should "be valid" in {
    Or(True, False).simplify shouldEqual True
    Or(Var("a"), Or(Var("b"), Or(Var("c"), Var("d")))).simplify shouldEqual Or(Var("a"), Or(Var("b"), Or(Var("c"), Var("d")))).simplify
  }


  "And" should "be valid" in {
    And(True, False).simplify shouldEqual False
    And(Var("a"), And(Var("b"), And(Var("c"), Var("d")))).simplify shouldEqual And(Var("a"), And(Var("b"), And(Var("c"), Var("d")))).simplify
  }

  "Impl" should "be valid" in {
    Impl(Var("a"), Var("b")).simplify shouldEqual Or(Not(Var("a")), Var("b"))
  }

  "Eq" should "be valid" in {
    // a <=> b -> (!a | b) & (a | !b)
    Eq(Var("a"), Var("b")).simplify shouldEqual And(Or(Not(Var("a")), Var("b")), Or(Var("a"), Not(Var("b")))).simplify
  }

  "C" should "simplify" in {
    C(Not(Var("a"))).simplify shouldEqual C(Var("a")).simplify
  }

  "N" should "simplify" in {
    N(And(Var("a"), Var("b"))).simplify shouldEqual And(N(Var("a")), N(Var("b"))).simplify
    N(Or(Var("a"), Var("b"))).simplify shouldEqual Or(N(Var("a")), N(Var("b"))).simplify
    N(Impl(Var("a"), Var("b"))).simplify shouldEqual Or(Not(N(Var("a"))), N(Var("b"))).simplify
    N(Eq(Var("a"), Var("b"))).simplify shouldEqual Eq(N(Var("a")), N(Var("b"))).simplify
    N(Not(False)).simplify shouldEqual Not(N(False)).simplify
  }


}
