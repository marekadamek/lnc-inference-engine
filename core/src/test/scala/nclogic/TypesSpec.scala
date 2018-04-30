package nclogic

import nclogic.java.model.expr._
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
    Neg(True).simplify shouldEqual False
    Neg(False).simplify shouldEqual True
    Neg(Neg(Var("a"))).simplify shouldEqual Var("a")
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
    Impl(Var("a"), Var("b")).simplify shouldEqual Or(Neg(Var("a")), Var("b"))
  }

  "Eq" should "be valid" in {
    // a <=> b -> (!a | b) & (a | !b)
    Eq(Var("a"), Var("b")).simplify shouldEqual And(Or(Neg(Var("a")), Var("b")), Or(Var("a"), Neg(Var("b")))).simplify
  }

  "C" should "simplify" in {
    Change(Neg(Var("a"))).simplify shouldEqual Change(Var("a")).simplify
  }

  "N" should "simplify" in {
    Next(And(Var("a"), Var("b"))).simplify shouldEqual And(Next(Var("a")), Next(Var("b"))).simplify
    Next(Or(Var("a"), Var("b"))).simplify shouldEqual Or(Next(Var("a")), Next(Var("b"))).simplify
    Next(Impl(Var("a"), Var("b"))).simplify shouldEqual Or(Neg(Next(Var("a"))), Next(Var("b"))).simplify
    Next(Eq(Var("a"), Var("b"))).simplify shouldEqual Eq(Next(Var("a")), Next(Var("b"))).simplify
    Next(Neg(False)).simplify shouldEqual Neg(Next(False)).simplify
  }


}
