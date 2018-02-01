package nclogic.model.converters

import nclogic.model.expr._
import org.scalatest._


class DnfConverterSpec extends FlatSpec with Matchers {

  val cA = Term("a")
  val cB = Term("b")
  val cC = Term("c")
  val cD = Term("d")
  val cE = Term("e")
  val cF = Term("f")

  "DnfConverter" should "convert 1" in {
    val given = cA & cB & (cC | cD)
    val expected = (cA | cC) & (cA | cD) & (cB | cC) & (cB | cD)
    val actual = DnfConverter.convert(given)
    println(actual)
    actual shouldEqual expected
  }

}
