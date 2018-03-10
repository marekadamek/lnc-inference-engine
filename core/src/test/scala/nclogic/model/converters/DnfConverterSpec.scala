package nclogic.model.converters

import nclogic.model.expr._
import org.scalatest._


class DnfConverterSpec extends FlatSpec with Matchers {

  val cA = Var("a")
  val cB = Var("b")
  val cC = Var("c")
  val cD = Var("d")
  val cE = Var("e")
  val cF = Var("f")

  "DnfConverter" should "convert 1" in {
    val given = cA & cB & (cC | cD)
    val expected = (cA | cC) & (cA | cD) & (cB | cC) & (cB | cD)
    val actual = DnfConverter.convert(given)
    println(actual)
    actual shouldEqual expected
  }

}
