package nclogic.model.expr.ltl

import nclogic.model.expr._
import org.scalatest.{FlatSpec, Matchers}

class UntilTest extends FlatSpec with Matchers {

  behavior of "Until"

  it should "convert to LNC" in {
    val p = Var("p")
    val q = Var("q")

    Until(p, q).toLNC(3) shouldEqual Expr.or(
      q,
      Expr.and(p, N(q)),
      Expr.and(p, N(p), N(2, q)),
      Expr.and(p, N(p), N(2, p), N(3, q))
    )
  }
}
