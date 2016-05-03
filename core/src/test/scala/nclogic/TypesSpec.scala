package nclogic

import nclogic.model.Types._
import org.scalatest._

class TypesSpec extends FlatSpec with Matchers {

  "Const" should "be valid" in {
    Const(true).simplify shouldEqual Const(true)
    Const(false).simplify shouldEqual Const(false)
  }

  "Var" should "be valid" in {
    Var("a").simplify shouldEqual Var("a")
  }

  "Neg" should "be valid" in {
    Neg(Const(true)).simplify shouldEqual Const(false)
    Neg(Const(false)).simplify shouldEqual Const(true)
    Neg(Neg(Var("a"))).simplify shouldEqual Var("a")
  }

  "Or" should "be valid" in {
    Or(Const(true), Const(false)).simplify shouldEqual Const(true)
    Or(Var("a"), Or(Var("b"), Or(Var("c"), Var("d")))).simplify shouldEqual Or(Var("a"), Or(Var("b"), Or(Var("c"), Var("d"))))
  }


  "And" should "be valid" in {
    And(Const(true), Const(false)).simplify shouldEqual Const(false)
    And(Var("a"), And(Var("b"), And(Var("c"), Var("d")))).simplify shouldEqual And(Var("a"), And(Var("b"), And(Var("c"), Var("d"))))
  }

  "Impl" should "be valid" in {
    Impl(Var("a"), Var("b")).simplify shouldEqual Or(Neg(Var("a")), Var("b"))
  }

  "Eq" should "be valid" in {
    // a <=> b -> (a & b) | (!a | b)
    Eq(Var("a"), Var("b")).simplify shouldEqual Or(And(Var("a"), Var("b")), And(Neg(Var("a")), Neg(Var("b"))))
  }

  "C" should "simplify" in {
    C(Neg(Var("a"))).simplify shouldEqual C(Var("a")).simplify
  }

  "N" should "simplify" in {
    N(And(Var("a"), Var("b"))).simplify shouldEqual And(N(Var("a")), N(Var("b")))
    N(Or(Var("a"), Var("b"))).simplify shouldEqual Or(N(Var("a")), N(Var("b")))
    N(Impl(Var("a"), Var("b"))).simplify shouldEqual Or(Neg(N(Var("a"))), N(Var("b")))
    N(Eq(Var("a"), Var("b"))).simplify shouldEqual Eq(N(Var("a")), N(Var("b"))).simplify
    N(Neg(Const(false))).simplify shouldEqual Neg(N(Const(false)))
  }


}
