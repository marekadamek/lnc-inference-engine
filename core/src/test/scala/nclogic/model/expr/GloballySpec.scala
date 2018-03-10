package nclogic.model.expr

import org.scalatest.{FlatSpec, Matchers}

class GloballySpec extends FlatSpec with Matchers {

  val cA = Var("a")
  val cB = Var("b")
  val cC = Var("c")
  val cD = Var("d")
  val cE = Var("e")
  val cF = Var("f")

  "Globally" should "simplify 1" in {
    val given = Globally(Or(cA, cB))
    val expected = Expr.and(Or(cA, cB), Next(Or(cA, cB)), Next(Next(Or(cA, cB))))
    val actual = given.simplify(2)
    actual shouldEqual expected.simplify
  }

}


