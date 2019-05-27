package lnc

import lnc.expr._
import lnc.expr.converters.PrefixFormulaConverter
import lnc.mc.LNCModel
import lnc.sat.SatSolvers

object Proves extends App {

  import LNCModel._
  import PrefixFormulaConverter._
  import lnc.expr.converters.NormalFormConverter._

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

    // LNC (non-axioms)
    , N(a) <-> (a <-> !C(a))
    , C(a) <-> (a <-> !N(a))
    , C(a & b) -> (C(a) | C(b))
    , C(a <-> b) -> !(C(a) <-> C(b))
  )

  val counterTautologies = List(
    //KRZ
    (!a -> !b) <-> !(b -> a)

    //LNC
    , (!a -> !N(a)) <-> !(N(a) -> a)
    , a & !C(a) & C(b) & !C(a -> b)
  )

  val (result, tm) = time.measureTime {
    val tautologiesByCycleDetectionOk = tautologies.forall(t => {
      findCycle(Not(t), SatSolvers.tableAux).isEmpty
    })

    val counterTautologiesByCycleDetection = tautologies.forall(t => {
      findCycle(Not(t), SatSolvers.tableAux).isEmpty
    })

    val tautologiesByPrefixFormulaSATOk = tautologies.forall(t => {
      val pf = prefixFormula(convertToNormalForm(Not(t)))
      SatSolvers.tableAux.getSolution(pf).isEmpty
    })

    val counterTautologiesByPrefixFormulaSATOk = counterTautologies.forall(ct => {
      val pf = prefixFormula(convertToNormalForm(ct))
      SatSolvers.tableAux.getSolution(pf).isEmpty
    })

    List(
      tautologiesByCycleDetectionOk,
      counterTautologiesByCycleDetection,
      tautologiesByPrefixFormulaSATOk,
      counterTautologiesByPrefixFormulaSATOk
    ).forall(x => x)
  }


  println("Is ok (?): " + result)
  println("Time (s): " + tm.seconds)
}

