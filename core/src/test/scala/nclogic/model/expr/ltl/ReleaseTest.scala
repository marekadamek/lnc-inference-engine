package nclogic.model.expr.ltl

import nclogic.model.expr._
import org.scalatest.{FlatSpec, Matchers}

class ReleaseTest extends FlatSpec with Matchers {

  behavior of "Release"

  it should "convert to LNC" in {
    val p = Var("p")
    val q = Var("q")

    Release(p, q).toLNC(3) shouldEqual Expr.and(
      q,
      Expr.or(p, N(q)),
      Expr.or(p, N(p), N(2, q)),
      Expr.or(p, N(p), N(2, p), N(3, q))
    )
  }
}
