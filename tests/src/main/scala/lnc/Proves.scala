package lnc

import lnc.expr._
import lnc.expr.converters.PrefixFormulaConverter
import lnc.mc.LNCModel
import lnc.sat.SatSolvers

object Proves extends App {

  import LNCModel._
  import PrefixFormulaConverter._
  import lnc.expr.converters.NormalFormConverter._

  val p = Var("a")
  val q = Var("b")
  val r = Var("c")

//  def toLatex(e: Expr): String = e match {
//    case Var(p) => p
//    case Not(x) => "\\lnot " + toLatex(x)
//    case And(es) => "(" + es.map(toLatex).mkString(" \\land ") + ")"
//    case Or(es) => "(" + es.map(toLatex).mkString(" \\lor ") + ")"
//    case Impl(e1, e2) => "(" + toLatex(e1) + " \\leftarrow " + toLatex(e2) + ")"
//    case Eq(e1, e2) => "(" + toLatex(e1) + " \\leftrightarrow " + toLatex(e2) + ")"
//    case Next(e, l) => "N" + toLatex(e)
//    case Change(e, l) =>"C" + toLatex(e)
//  }

  val tautologies = List(
    // KRZ
    p | !p
    , p -> (p | q)
    , (p & q) -> p
    , ((p -> q) & (q -> r)) -> (p -> r)
    , ((p -> r) & (q -> r)) -> ((p | q) -> r)
    , ((p -> q) & (p -> r)) -> (p -> (q & r))
    , ((p -> q) & (p -> !q)) -> !p
    , ((p -> q) & p) -> q
    , (p & (q | r)) -> ((p & q) | (p & r))
    , !(p & q) -> (!p | !q)

    // LNC (axioms)
    , !N(p) <-> N(!p)
    , N(p -> q) <-> (N(p) -> N(q))
    , C(p) <-> C(!p)
    , C(p) -> (p -> N(!p))
    , (p & N(!p)) -> C(p)
    , (!p & N(p)) -> C(p)
    , N(C(p)) <-> C(N(p))

    // LNC (non-axioms)
    , N(p) <-> (p <-> !C(p))
    , C(p) <-> (p <-> !N(p))
    , C(p & q) -> (C(p) | C(q))
    , C(p <-> q) -> !(C(p) <-> C(q))
  )

  val counterTautologies = List(
    //KRZ
    (!p -> !q) <-> !(q -> p)

    //LNC
    , (!p -> !N(p)) <-> !(N(p) -> p)
    , p & !C(p) & C(q) & !C(p -> q)
  )

  //tautologies.map(toLatex).foreach(println)
  //counterTautologies.map(toLatex).foreach(println)

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

