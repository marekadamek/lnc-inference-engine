package nclogic.model.converters

import nclogic.java.model.expr._
import org.scalatest._


class CnfConverterSpec extends FlatSpec with Matchers {

  val b = Var("b")
  val c = Var("c")
  val d = Var("d")
  val e = Var("e")
  val f = Var("f")
  val g = Var("g")

  "CnfConverter" should "convert 1" in {
    val given = (g & b) | (c & d)
    val expected = (g | c) & (g | d) & (b | c) & (b | d)
    val actual = CnfConverter.convert(given)
    actual shouldEqual expected.simplify
  }

  it should "convert 2" in {
    val given = g | (c & d)
    val expected = (g | c) & (g | d)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected.simplify
  }

  it should "convert 3" in {
    val given = (g & b) | c
    val expected = (g | c) & (b | c)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected.simplify
  }

  it should "convert 4" in {
    val given = (g & b & c) | d
    val expected = (g | d) & (b | d) & (c | d)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected.simplify
  }

  it should "convert 5" in {
    val given = (g & b) | (c & d) | (e & f)
    val expected = (g | c | e) & (g | c | f) & (g | d | e) & (g | d | f) & (b | c | e) & (b | c | f) & (b | d | e) & (b | d | f)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected.simplify
  }

  it should "convert 6" in {
    val given = (g & b & c) | (d & e & f)
    val expected = (g | d) & (g | e) & (g | f) & (b | d) & (b | e) & (b | f) & (c | d) & (c | e) & (c | f)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected.simplify
  }

  it should "convert 7" in {
    val given = (g & b) | (g & c)
    val expected = g & (b | c)
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected.simplify
  }

  it should "convert 8" in {
    val given = (g & N(g)) | (G(g) & g)
    val expected = (G(g) | g) & (G(g) | N(g))
    val actual = CnfConverter.convert(given)

    actual shouldEqual expected.simplify
  }
}
