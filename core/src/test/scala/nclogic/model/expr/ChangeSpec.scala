package nclogic.model.expr

import org.scalatest.{FlatSpec, Matchers}

class NextSpec extends FlatSpec with Matchers {

  val cA = Var("a")
  val cB = Var("b")
  val cC = Var("c")
  val cD = Var("d")
  val cE = Var("e")
  val cF = Var("f")

  "Next" should "simplify 1" in {
    val given = !N(cA)
    val expected = !N(cA)
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 2" in {
    val given = N(N(cA))
    val expected = N(cA, 2)
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 3" in {
    val given = !N(!cA, 6)
    val expected = N(cA, 6)
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 4" in {
    val given = N(!cA)
    val expected = !N(cA)
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 5" in {
    val given = !N(cA & cB)
    val expected = !N(cA) | !N(cB)
    val actual = given.simplify
    actual shouldEqual expected
  }
//
//  it should "simplify 4" in {
//    val given = And(cA, cB, cC, cD, cE, cF, True)
//    val expected = And(cA, cB, cC, cD, cE, cF)
//    val actual = given.simplify
//    actual shouldEqual expected
//  }

//  it should "simplify 6" in {
//    val given = And(cA, Or(cA, cB))
//    val expected = cA
//    val actual = given.simplify
//    actual shouldEqual expected.simplify
//  }

}
