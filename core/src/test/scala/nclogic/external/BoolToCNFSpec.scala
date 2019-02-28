package nclogic.external

import nclogic.model.expr._
import org.scalatest.{FlatSpec, Matchers}

class BoolToCNFSpec extends FlatSpec with Matchers {

  val cA = Var("a")
  val cB = Var("b")
  val cC = Var("c")
  val cD = Var("d")
  val cE = Var("e")
  val cF = Var("f")

  behavior of "BoolToCNF"

  it should "convert basic formula" in {
    val e = !(cA & cB) -> (cC | cD)
    val actual = e.boolString
    actual shouldEqual "(!(a & b) -> (c | d))"
  }

  it should "convert !N^2a" in {
    val e = !N(2,cA)
    val actual = e.boolString
    actual shouldEqual "!X_2_a"
  }
}
