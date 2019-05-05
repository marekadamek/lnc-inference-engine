package lnc.expr

import org.scalatest.{FlatSpec, Matchers}

class ChangeSpec extends FlatSpec with Matchers {

  val cA = Var("a")

  "Change" should "simplify C" in {
    val given = C(cA)
    val expected =  (cA & !N(cA)) | (!cA & N(cA))
    val actual = given.simplify
    actual shouldEqual expected.simplify

  }

  it should "simplify 1" in {
    val given = C(2, cA)
    val expected = (((cA & !N(cA)) | (!cA & N(cA))) & (!N(cA) | N(2, cA)) & (N(cA) | !N(2, cA))) | ((!cA | N(cA)) & (cA | !N(cA)) & ((N(cA) & !N(2, cA)) | (!N(cA) & N(2, cA))))
    val actual = given.simplify
    actual shouldEqual expected.simplify

  }

  it should "simplify 2" in {
    val given = cA & C(cA)
    val expected = cA & !N(cA)
    given.simplify shouldEqual expected.simplify
  }

}
