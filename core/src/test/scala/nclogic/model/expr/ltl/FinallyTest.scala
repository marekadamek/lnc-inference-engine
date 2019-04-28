package nclogic.model.expr.ltl

import nclogic.model.expr._
import org.scalatest.{FlatSpec, Matchers}

class FinallyTest extends FlatSpec with Matchers {

  behavior of "Finally"

  it should "convert to LNC" in {
    val v = Var("v")
    Finally(v).toLNC(5) shouldEqual Expr.or(v, N(v), N(2, v), N(3, v), N(4, v), N(5, v))
  }
}
