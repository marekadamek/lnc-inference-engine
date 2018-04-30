package nclogic.java.model.expr

import org.scalatest.{FlatSpec, Matchers}

class OrSpec extends FlatSpec with Matchers {

  val cA = Var("a")
  val cB = Var("b")
  val cC = Var("c")
  val cD = Var("d")
  val cE = Var("e")
  val cF = Var("f")

  "Or" should "simplify 1" in {
    val given = Or(Or(cA, cB), cC, Or(cD, cE, cF))
    val expected = Or(cA, cB, cC, cD, cE, cF)
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 2" in {
    val given = Or(cA)
    val expected = cA
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 3" in {
    val given = Or(cA, cB, cC, cD, cE, cF, True)
    val expected = True
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 4" in {
    val given = Or(cA, cB, cC, cD, cE, cF, False)
    val expected = Or(cA, cB, cC, cD, cE, cF)
    val actual = given.simplify
    actual shouldEqual expected
  }

  it should "simplify 6" in {
    val given = Or(cA, And(cA, cB))
    val expected = cA
    val actual = given.simplify
    actual shouldEqual expected.simplify
  }

}


