package lnc.expr

import org.scalatest.{FlatSpec, Matchers}

class ExprTest extends FlatSpec with Matchers {

  "Expr" should "iterate over" in {
    val cA = Var("a")
    val cB = Var("b")

    val e = Eq(And(Set[Expr](cA, cB)), Next(Or(Set[Expr](cB, cA)), 3))

    val list = new PrefixExprVisitor(e).toList

    list shouldEqual List(
      Eq(And(Set[Expr](cA, cB)), Next(Or(Set[Expr](cB, cA)), 3)),
      And(Set[Expr](cA, cB)),
      cA,
      cB,
      Next(Or(Set[Expr](cB, cA)), 3),
      Or(Set[Expr](cB, cA)),
      cB,
      cA
    )
  }
}
