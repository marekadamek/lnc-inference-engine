package nclogic

import nclogic.model.expr._

object Proves extends App {

  val a = Var("a")
  val b = Var("b")
  val c = Var("c")

  val tautologies = List(
    // KRZ
    a | !a
    , a -> (a | b)
    , (a & b) -> a
    , ((a -> b) & (b -> c)) -> (a -> c)
    , ((a -> c) & (b -> c)) -> ((a | b) -> c)
    , ((a -> b) & (a -> c)) -> (a -> (b & c))
    , ((a -> b) & (a -> !b)) -> !a
    , ((a -> b) & a) -> b
    , (a & (b | c)) -> ((a & b) | (a & c))
    , !(a & b) -> (!a | !b)

    // LNC (axioms)
    , !N(a) <-> N(!a)
    , N(a -> b) <-> (N(a) -> N(b))
    , C(a) <-> C(!a)
    , C(a) -> (a -> N(!a))
    , (a & N(!a)) -> C(a)
    , (!a & N(a)) -> C(a)
    , N(C(a)) <-> C(N(a))


    // LNC (not axioms)
    , N(a) <-> (a <-> !C(a))
    , C(a) <-> (a <-> !N(a))
    , C(a & b) -> (C(a) | C(b))
    , C(a <-> b) -> (C(a) ^ C(b))
  )

  val counterTautologies = List(
    //KRZ
    (!a -> !b) <-> !(b -> a)

    //LNC
    , (!a -> !N(a)) <-> !(N(a) -> a)
    , a & !C(a) & C(b) & !C(a -> b)
  )

  val result = time.measureTime {
    tautologies.forall(isTautology) && counterTautologies.forall(isContraTautology)
  }

  println("Is ok (?): " + result.result)
  println("Time (ms): " + result.millis)
}

