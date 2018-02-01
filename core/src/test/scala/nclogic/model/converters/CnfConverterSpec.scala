package nclogic.model.converters

import nclogic.model.expr._
import org.scalatest._


class CnfConverterSpec extends FlatSpec with Matchers {

  val cA = Term("a")
  val cB = Term("b")
  val cC = Term("c")
  val cD = Term("d")
  val cE = Term("e")
  val cF = Term("f")

  "CnfConverter" should "convert 1" in {
    val given = (cA & cB) | (cC & cD)
    val expected = (cA | cC) & (cA | cD) & (cB | cC) & (cB | cD)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected
  }

  it should "convert 2" in {
    val given = cA | (cC & cD)
    val expected = (cA | cC) & (cA | cD)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected
  }

  it should "convert 3" in {
    val given = (cA & cB) | cC
    val expected = (cA | cC) & (cB | cC)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected
  }

  it should "convert 4" in {
    val given = (cA & cB & cC) | cD
    val expected = (cA | cD) & (cB | cD) & (cC | cD)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected
  }

  it should "convert 5" in {
    val given = (cA & cB) | (cC & cD) | (cE & cF)
    val expected = (cA | cC | cE) & (cA | cC | cF) & (cA | cD | cE) & (cA | cD | cF) & (cB | cC | cE) & (cB | cC | cF) & (cB | cD | cE) & (cB | cD | cF)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected
  }

  it should "convert 6" in {
    val given = (cA & cB & cC) | (cD & cE & cF)
    val expected = (cA | cD) & (cA | cE) & (cA | cF) & (cB | cD) & (cB | cE) & (cB | cF) & (cC | cD) & (cC | cE) & (cC | cF)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected
  }

  it should "convert 7" in {
    val given = (cA & cB) | (cA & cC)
    val expected = cA & (cB | cC)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected
  }

  it should "convert 8" in {
    val given = (cA & N(!cA & !cB & cC) & N(N(cD))) | (cB & N(!cA & !cB & cC) & N(N(cE)))
    val expected = (cA | cB) & (cA | N(N(cE))) & N(!cA) & N(!cB) & N(cC) & (N(N(cD)) | cB) & (N(N(cD)) | N(N(cE)))
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected
  }
}
