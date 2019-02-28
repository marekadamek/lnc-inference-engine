package nclogic.model.expr

import org.scalatest.{FlatSpec, Matchers}

class AndSpec extends FlatSpec with Matchers {

  val cA = Var("a")
  val cB = Var("b")
  val cC = Var("c")
  val cD = Var("d")
  val cE = Var("e")
  val cF = Var("f")

  "And" should "simplify 1" in {
    val given = And(And(cA, cB), cC, And(cD, cE, cF))
    val expected = And(cA, cB, cC, cD, cE, cF)
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 2" in {
    val given = And(cA)
    val expected = cA
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 3" in {
    val given = And(cA, cB, cC, cD, cE, cF, False)
    val expected = False
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 4" in {
    val given = And(cA, cB, cC, cD, cE, cF, True)
    val expected = And(cA, cB, cC, cD, cE, cF)
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 5" in {
    val given = And(cA, cB, cC, cD, cE, cF, !cA)
    val expected = False
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 6" in {
    val given = And(N(cA), cB, cC, cD, cE, cF, !N(cA))
    val expected = False
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 7" in {
    val given = cA & (cB | cC)
    val expected = (cA & cB) | (cA & cC)
    val actual = given.simplify
    actual shouldEqual expected
  }

  //  it should "simplify 8" in {
  //    val given = cA & (cA | cB)
  //    val expected = cA
  //    val actual = given.simplify
  //    actual shouldEqual expected.simplify
  //  }

}
