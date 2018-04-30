package nclogic

import nclogic.binary.BinaryValidator.toBool
import nclogic.binary.{BinaryGenerator, BinaryValidator, BinaryValidatorWithCorrection}
import nclogic.model.converters.CnfConverter
import nclogic.java.model.expr._
import nclogic.model.{BinaryGraph, HistoryGraph, HistoryGraphUtils}
import nclogic.sat.Sat

import scala.util.Random


object CorrectionCyclesTest extends App {

  val a = Var("a")
  val b = Var("b")
  val c = Var("b")

  //val (formula, startNode) = (!a -> N(N(a)), a & N(a))
  //val (formula, startNode) = (C(a | b), !a & !b)
  val (formula, startNode) = ((!a -> C(b)) & (!b -> C(a)), a & !b)

  val sat = Sat.solve(CnfConverter.convert(formula))
  val graph = HistoryGraph(sat)
  val bGraph = BinaryGraph(graph)

  val bStartNode = startNode.getTerms.map(toBool)

  def toString(list: List[Boolean]) = list.map(e => if (e) 1 else 0).mkString("")

  def applyNoise(list: List[Boolean], ber: Float): List[Boolean] = {
    val toAlter = (list.length * ber).toInt
    val indices = Random.shuffle(list.indices.toList).take(toAlter)

    def mod(list: List[Boolean], indices: List[Int]): List[Boolean] = indices match {
      case Nil => list
      case i :: is =>
        val elem = list(i)
        mod(list.updated(i, !elem), is)
    }

    mod(list, indices)
  }

  val All = 1000
  val length = 10000

  val cycles = HistoryGraphUtils.getCycles(graph, startNode)

  def cycleToBool(cycle: List[Expr]): List[Boolean] = cycle match {
    case Nil => Nil
    case n :: ns => n.getTerms.toList.take(graph.baseTerms.size).map(toBool) ++ cycleToBool(ns)
  }

  val bCycles = cycles.map(cycleToBool)

  println("Length: " + BinaryGenerator.generateCycles(length, graph, bCycles, bStartNode.toList).length)

  def genNoise(B: List[Boolean], ber: Float): List[Boolean] = {
    val BM = applyNoise(B, ber)
    if (BinaryValidator.validateCycles(BM, bCycles, bStartNode.toList))
      genNoise(B, ber)
    else BM
  }

  List(0.0001f, 0.00025f, 0.0005f, 0.00075f, 0.001f).foreach(ber => {
    var totalTime = 0L
    var errorDetected = 0
    var badRead = 0
    var goodRead = 0

    for (i <- 1 to All) {
      val B = BinaryGenerator.generateCycles(length, graph, bCycles, bStartNode.toList)
      val BM = genNoise(B, ber)

      val result = time.measureTime {
        BinaryValidatorWithCorrection.validateCyclesAndCorrect(BM, bCycles, bStartNode.toList)
      }

      if (result.result.isEmpty) {
        errorDetected += 1
      }
      else {
        if (result.result.get == B)
          goodRead += 1
        else
          badRead += 1
      }

      totalTime += result.microTime

    }

    println("cycles " + ": " + bCycles.size)
    println("BER " + ": " + ber)
    println("detections " + ": " + errorDetected)
    println("goodReads " + ": " + goodRead)
    println("bad read :( " + ": " + badRead)
    println("LNC time ms: " + totalTime / 1000)
    println()
  })
}

