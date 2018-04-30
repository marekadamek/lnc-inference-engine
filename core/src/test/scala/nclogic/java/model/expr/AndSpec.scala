package nclogic.java.model.expr

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

  "And" should "simplify 2" in {
    val given = And(cA)
    val expected = cA
    val actual = given.simplify
    actual shouldEqual expected
  }

  "And" should "simplify 3" in {
    val given = And(cA, cB, cC, cD, cE, cF, False)
    val expected = False
    val actual = given.simplify
    actual shouldEqual expected
  }

  "And" should "simplify 4" in {
    val given = And(cA, cB, cC, cD, cE, cF, True)
    val expected = And(cA, cB, cC, cD, cE, cF)
    val actual = given.simplify
    actual shouldEqual expected
  }

  "And" should "simplify 5" in {
    val given = And(cA, Globally(cA), Next(cA), cD, cE, cF)
    val expected = And(Globally(cA), cD, cE, cF)
    val actual = given.simplify
    actual shouldEqual expected.simplify
  }

  "And" should "simplify 6" in {
    val given = And(cA, Or(cA, cB))
    val expected = cA
    val actual = given.simplify
    actual shouldEqual expected.simplify
  }

}

object AndSpec {

}
