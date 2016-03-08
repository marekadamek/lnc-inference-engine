//package pl.edu.pw.elka.madamek.nclogic
//
//import org.scalatest._
//import pl.edu.pw.elka.madamek.nclogic.model.Types
//
//class TypesSpec extends FlatSpec with Matchers {
//  import Types._
//
//  "Const" should "be valid" in {
//    Const(true).simplify shouldEqual Const(true)
//    Const(false).simplify shouldEqual Const(false)
//  }
//
//  "Var" should "be valid" in {
//    Var("a").simplify shouldEqual Var("a")
//  }
//
//  "Neg" should "be valid" in {
//    Neg(Const(true)).simplify shouldEqual Const(false)
//    Neg(Const(false)).simplify shouldEqual Const(true)
//    Neg(Neg(Var("a"))).simplify shouldEqual Var("a")
//  }
//
//  "Or" should "be valid" in {
//    Or(Neg(Const(true)), Neg(Const(false))).simplify shouldEqual Or(Const(false), Const(true))
//    Or(Var("a"), Or(Var("b"), Or(Var("c"), Var("d")))).simplify shouldEqual Or(Var("a"), Var("b"), Var("c"), Var("d"))
//  }
//
//
//  "And" should "be valid" in {
//    And(Neg(Const(true)), Neg(Const(false))).simplify shouldEqual And(Const(false), Const(true))
//    And(Var("a"), And(Var("b"), And(Var("c"), Var("d")))).simplify shouldEqual And(Var("a"), Var("b"), Var("c"), Var("d"))
//  }
//
//  "Impl" should "be valid" in {
//    Impl(Var("a"), Var("b")).simplify shouldEqual Or(Neg(Var("a")), Var("b"))
//  }
//
//  "Eq" should "be valid" in {
//    // a <=> b -> (!a | b) & (!b | a)
//    Eq(Var("a"), Var("b")).simplify shouldEqual And(Or(Neg(Var("a")), Var("b")), Or(Neg(Var("b")), Var("a")))
//  }
//
//  "C" should "simplify" in {
//    C(Neg(Const(false))).simplify shouldEqual C(Const(true))
//  }
//
//  "N" should "simplify" in {
//    N(And(Var("a"), Var("b"))).simplify shouldEqual And(N(Var("a")), N(Var("b")))
//    N(Or(Var("a"), Var("b"))).simplify shouldEqual Or(N(Var("a")), N(Var("b")))
//    N(Impl(Var("a"), Var("b"))).simplify shouldEqual Impl(N(Var("a")), N(Var("b")))
//    N(Eq(Var("a"), Var("b"))).simplify shouldEqual Eq(N(Var("a")), N(Var("b")))
//    N(Neg(Const(false))).simplify shouldEqual N(Const(true))
//  }
//
//
//}
