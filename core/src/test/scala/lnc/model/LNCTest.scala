package lnc.model

import lnc.expr.{Var, _}
import org.scalatest.{FlatSpec, Matchers}

class LNCTest extends FlatSpec with Matchers {

  val cA = Var("a")
  val cB = Var("b")
  val cC = Var("c")
  val cD = Var("d")
  val cE = Var("e")
  val cF = Var("f")

  behavior of "LNC"

  import lnc.LNC._

  List(
    (cA, 0),
    (N(4,cA), 4),
    (N(4, cA) & C(8,!cA), 8),
    (C(C(N(3, cA))), 5)
  ) foreach {
    case (f, d) =>
      it should s"calculate depth of $f as $d" in {
        depth(f) shouldEqual d
      }
  }

//  List(
//    (!cA -> N(2, cA), !cA -> N(2, cA)),
//    (cA & N(2, cA), cA & N(cA) & N(2, cA)),
//    (cA & !N(2, cA), False)
//  ) foreach {
//    case (f, p) =>
//      it should s"calculate prefix of $f as $p" in {
//        prefixFormula(f) shouldEqual p.simplify
//      }
//
//  }
}
